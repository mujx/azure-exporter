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

import           Paths_azure_exporter           ( version )
import           Data.Version                   ( showVersion )

import qualified Options.Applicative           as Opt
import           Options.Applicative            ( (<**>) )
import           Control.Concurrent.STM
import           Control.Monad.Except
import           GHC.Generics
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as AT
import           Control.Monad.IO.Class         ( liftIO )
import qualified Control.Monad.Parallel        as P
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy          as LBS
import           Data.Either                    ( partitionEithers )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.String                   as S
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import qualified Data.Text.Lazy                as L
import           Network.HTTP.Types.Status      ( statusCode
                                                , status500
                                                )
import qualified Network.Wai                   as Wai
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout
                                                , logStdoutDev
                                                )
import           System.Environment             ( lookupEnv )
import           Text.Read                      ( readMaybe )
import           Web.Scotty.Trans
import           Network.HTTP.Client.Internal   ( HttpException(..)
                                                , HttpExceptionContent(..)
                                                , responseStatus
                                                )
import qualified Network.HTTP.Req              as R

import           Azure.Client
import           Azure.Config
import           Azure.Exporter

-- Data available on the metrics handler.
--
-- It is used to initially acquire the auth token or re-login in case the token expires,
-- and to perform the metric retrieval.
--
newtype AppState = AppState
  { clientConfig :: [ClientConfig]
    -- ^ Each config corresponds to a different Azure subscription.
  } deriving (Eq, Read, Show)

newtype AzureM a = AzureM
  { runAzureM :: ReaderT (TVar AppState) IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

-- Be explicit when we are operating at the 'AzureM' layer.
azureM :: MonadTrans t => AzureM a -> t AzureM a
azureM = lift

-- Helpers to make this feel more like a state monad.
gets :: (AppState -> b) -> AzureM b
gets f = f <$> (ask >>= liftIO . readTVarIO)

modify :: (AppState -> AppState) -> AzureM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

readListenPort :: Integer -> Text -> IO Integer
readListenPort defaultPort envVar =
  fromMaybe defaultPort . (readMaybe =<<) <$> lookupEnv (unpack envVar)

readRunEnv :: String -> String -> IO String
readRunEnv defaultEnv envVar = fromMaybe defaultEnv <$> lookupEnv envVar

validateConfig :: [ClientConfig] -> IO [ClientConfig]
validateConfig c = do
  withSecretsConf <- P.mapM mapSecrets c
  P.mapM mapLoginToken withSecretsConf


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
data ExporterError = AzError AzureError | AzApiError MetricsApiError
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
homeHandler :: ScottyT L.Text AzureM ()
homeHandler =
  get "/" $ html "<h1> Azure Exporter </h1> <a href='/metrics'>Metrics</a>"

metricsHandler :: ScottyT L.Text AzureM ()
metricsHandler = get "/metrics" $ do
  conf    <- azureM $ gets clientConfig
  results <- liftIO $ P.mapM (runExceptT . runHttpM . collectMetrics) conf

  let (errors, metricResults) = partitionEithers results

  -- The tokens expired. Try to re-login & obtain new tokens.
  if any is401 errors
    then do
      newConf <- liftIO $ P.mapM mapLoginToken conf
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
    -> ActionT L.Text AzureM ()
  sendResponse e v = if null e
    then text $ renderMetrics v
    else do
      liftIO $ print e

      status status500

      case head e of
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

runIO :: TVar AppState -> AzureM a -> IO a
runIO state m = runReaderT (runAzureM m) state

newtype CliOpts = CliOpts { optFile :: String }

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
      finalConf     <- validateConfig initialConf
      initialConfig <- newTVarIO (AppState { clientConfig = finalConf })

      scottyT port (runIO initialConfig) (app runEnv)
 where
  opts = Opt.info
    (cliOpts <**> Opt.helper)
    (  Opt.fullDesc
    <> Opt.progDesc
         "Web service that retrieves metrics from Azure and exports them for Prometheus."
    <> Opt.header ("Azure Exporter :: v" <> showVersion version)
    )

app :: String -> ScottyT L.Text AzureM ()
app runEnv = do
  -- Setup middlewares.
  middleware (setupLogger runEnv)

  -- Define routes.
  homeHandler
  metricsHandler
