{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Main
  ( main
  )
where

import           Data.Version                   ( showVersion )
import           Paths_azure_exporter           ( version )

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.IO.Class         ( liftIO )
import qualified Control.Monad.Parallel        as P
import           Control.Monad.Reader
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as AT
import qualified Data.ByteString.Lazy          as LBS
import           Data.Either                    ( partitionEithers )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.String                   as S
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text.Lazy                as L
import           GHC.Generics
import qualified Katip                         as K
import           Network.HTTP.Client.Internal   ( HttpException(..)
                                                , HttpExceptionContent(..)
                                                , responseStatus
                                                )
import qualified Network.HTTP.Req              as R
import           Network.HTTP.Types.Status      ( status500
                                                , statusCode
                                                )
import qualified Network.Wai                   as Wai
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout
                                                , logStdoutDev
                                                )
import qualified Options.Applicative           as Opt
import           Options.Applicative            ( (<**>) )
import           System.Environment             ( lookupEnv )
import           Text.Read                      ( readMaybe )
import           Web.Scotty.Trans

import           Azure.Client
import           Azure.Config
import           Azure.Exporter
import           Logger

-- Data available on the metrics handler.
--
-- It is used to initially acquire the auth token or re-login in case the token expires,
-- and to perform the metric retrieval.
--
data AppState = AppState
    { clientConfig :: [ClientConfig]
    -- ^ Each config corresponds to a different Azure subscription.
    , logEnv       :: K.LogEnv
    -- ^ Katip configuration.
    }

newtype AzureM m a = AzureM
    { runAzureM :: ReaderT (TVar AppState) m a
    } deriving ( Applicative
               , Functor
               , Monad
               , MonadIO
               , MonadReader (TVar AppState)
               )

instance (MonadIO m) => K.Katip (AzureM m) where
  getLogEnv = gets logEnv
  localLogEnv f (AzureM m) = do
    modify (\c -> c { logEnv = f $ logEnv c })
    AzureM m

-- Be explicit when we are operating at the 'AzureM' layer.
azureM :: (MonadTrans t, Monad m) => AzureM m a -> t (AzureM m) a
azureM = lift

-- Helpers to make this feel more like a state monad.
gets :: MonadIO m => (AppState -> b) -> AzureM m b
gets f = f <$> (ask >>= liftIO . readTVarIO)

modify :: MonadIO m => (AppState -> AppState) -> AzureM m ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

readListenPort :: Integer -> Text -> IO Integer
readListenPort defaultPort envVar =
  fromMaybe defaultPort . (readMaybe =<<) <$> lookupEnv (unpack envVar)

readRunEnv :: String -> String -> IO String
readRunEnv defaultEnv envVar = fromMaybe defaultEnv <$> lookupEnv envVar

validateConfig :: K.LogEnv -> [ClientConfig] -> IO [ClientConfig]
validateConfig le c = do
  withSecretsConf <- P.mapM (mapSecrets le) c
  P.mapM (mapLoginToken le) withSecretsConf

-- Check if the Http exception contains an 401 status code.
is401 :: R.HttpException -> Bool
is401 e = case e of
  R.VanillaHttpException (HttpExceptionRequest _ (StatusCodeException res _))
    -> statusCode (responseStatus res) == 401
  _ -> False

collectMetrics :: ClientConfig -> HttpM [MetricValueResponse]
collectMetrics conf = do
  let subId = subscriptionId conf
  let tkn   = fromMaybe "" (loginToken conf)
  P.mapM (getMetricValue subId tkn) (resources conf)

-- Azure responds with one of these two types when it encounters an error.
data ExporterError
    = AzError AzureError
    | AzApiError MetricsApiError
    deriving (Show, Generic)

untaggedOptions :: AT.Options
untaggedOptions = AT.defaultOptions { AT.sumEncoding = AT.UntaggedValue }

instance A.FromJSON ExporterError where
  parseJSON = A.genericParseJSON untaggedOptions

instance A.ToJSON ExporterError where
  toJSON = A.genericToJSON untaggedOptions

--
-- Handlers
--
homeHandler :: ScottyT L.Text (AzureM IO) ()
homeHandler =
  get "/" $ html "<h1> Azure Exporter </h1> <a href='/metrics'>Metrics</a>"

metricsHandler :: ScottyT L.Text (AzureM IO) ()
metricsHandler = get "/metrics" $ do
  conf <- azureM $ gets clientConfig

  azureM $ logMs
    K.DebugS
    (  pack
    $  "Preparing to collect metrics from "
    <> show (length conf)
    <> " subscriptions"
    )

  results <- liftIO $ P.mapM (runExceptT . runHttpM . collectMetrics) conf

  let (errors, metricResults) = partitionEithers results

  let totalErrors             = length errors
  let totalQueries            = length results

  azureM $ logMs
    K.DebugS
    (  pack
    $  "Received "
    <> show totalErrors
    <> " errors from total of "
    <> show totalQueries
    <> " queries - "
    <> show ((1 - totalErrors `quot` totalQueries) * 100)
    <> "% success."
    )

  if any is401 errors
    then do
      azureM $ logMs K.WarningS "There are expired tokens. Trying to re-login."

      le      <- azureM K.getLogEnv
      newConf <- liftIO $ P.mapM (mapLoginToken le) conf
      azureM $ modify $ \c -> c { clientConfig = newConf }

      newResults <- liftIO
        $ P.mapM (runExceptT . runHttpM . collectMetrics) newConf
      let (errors', metricResults') = partitionEithers newResults

      sendResponse errors' metricResults'
    else sendResponse errors metricResults
 where
  sendResponse
    :: [R.HttpException]
  -- ^ The first error encountered for each subscription.
    -> [[MetricValueResponse]]
  -- ^ A list of metrics for each resource on each subscription
    -> ActionT L.Text (AzureM IO) ()
  sendResponse e v = if null e
    then text $ renderMetrics v
    else do
      let firstErr = head e

      azureM $ logMs K.WarningS (pack $ show firstErr)

      status status500

      case firstErr of
        R.VanillaHttpException (HttpExceptionRequest _ (StatusCodeException _ b))
          -> json (A.decode (LBS.fromStrict b) :: Maybe ExporterError)
        R.JsonHttpException jsonError -> json $ AzureError
          { azError = MetricsApiError { code    = "JsonHttpException"
                                      , message = pack jsonError
                                      }
          }
        _ -> json $ AzureError
          { azError =
            MetricsApiError
              { code    = "UnknownError"
              , message =
                "Something went wrong. Check the logs for more details."
              }
          }

setupLogger :: (Eq a, S.IsString a) => a -> Wai.Middleware
setupLogger runEnv = if runEnv == "production" then logStdout else logStdoutDev

setupLogEnv :: (Eq a, S.IsString a) => a -> IO K.LogEnv
setupLogEnv runEnv = if runEnv == "production" then prodEnv else devEnv

runIO :: TVar AppState -> AzureM IO a -> IO a
runIO state m = runReaderT (runAzureM m) state

newtype CliOpts = CliOpts
    { optFile :: String
    }

cliOpts :: Opt.Parser CliOpts
cliOpts = CliOpts <$> Opt.strOption
  (  Opt.long "config-file"
  <> Opt.short 'f'
  <> Opt.help "Exporter settings"
  <> Opt.metavar "FILE"
  <> Opt.showDefault
  <> Opt.value "config.yaml"
  )


main :: IO ()
main = do
  parsedOpts <- Opt.execParser opts
  port       <- fromInteger <$> readListenPort 3000 "PORT"
  runEnv     <- readRunEnv "dev" "ENV"
  confFile   <- loadConfigFile (optFile parsedOpts)
  case confFile of
    Left  err         -> error err
    Right initialConf -> do
      logenv        <- setupLogEnv runEnv
      validConf     <- validateConfig logenv initialConf
      initialConfig <- newTVarIO
        (AppState { clientConfig = validConf, logEnv = logenv })

      scottyT port (runIO initialConfig) (app runEnv)
 where
  opts = Opt.info
    (cliOpts <**> Opt.helper)
    (  Opt.fullDesc
    <> Opt.progDesc
         "Web service that retrieves metrics from Azure and exports them for Prometheus."
    <> Opt.header ("Azure Exporter :: v" <> showVersion version)
    )

app :: String -> ScottyT L.Text (AzureM IO) ()
app runEnv = do
  -- Setup middlewares.
  middleware (setupLogger runEnv)

  -- Define routes.
  homeHandler
  metricsHandler
