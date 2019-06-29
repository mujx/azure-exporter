{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Main
  ( main
  )
where

import           Data.Version                   ( showVersion )
import           Paths_azure_exporter           ( version )

import           System.Exit                    ( die )
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Exception              ( throw )
import           Control.Monad.Reader.Class
import           Control.Monad.IO.Class         ( liftIO )
import qualified Control.Monad.Parallel        as P
import           Control.Monad.Reader          as MR
import qualified Data.Aeson                    as A
import           Data.Aeson                     ( (.=) )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import qualified Data.Aeson.Types              as AT
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Either                    ( partitionEithers )
import           Data.Maybe                     ( fromMaybe )
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

newtype AppMonad m a = AppMonad
    { runAppM :: ReaderT (TVar AppState) m a
    } deriving ( Applicative
               , Functor
               , Monad
               , MonadIO
               , MonadReader (TVar AppState)
               )

instance (MonadIO m) => K.Katip (AppMonad m) where
  getLogEnv = gets logEnv
  localLogEnv f (AppMonad m) = do
    modify (\c -> c { logEnv = f $ logEnv c })
    AppMonad m

instance (MonadIO m) => R.MonadHttp (AppMonad m) where
    handleHttpException = throw

appM :: (MonadTrans t, Monad m) => AppMonad m a -> t (AppMonad m) a
appM = lift

gets :: MonadIO m => (AppState -> b) -> AppMonad m b
gets f = f <$> (ask >>= liftIO . readTVarIO)

modify :: MonadIO m => (AppState -> AppState) -> AppMonad m ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

readListenPort :: Integer -> Text -> IO Integer
readListenPort defaultPort envVar =
  fromMaybe defaultPort . (readMaybe =<<) <$> lookupEnv (unpack envVar)

readRunEnv :: String -> IO EnvType
readRunEnv envVar = toEnvType <$> lookupEnv envVar
 where
  toEnvType :: Maybe String -> EnvType
  toEnvType (Just "production") = Prod
  toEnvType _                   = Dev

validateConfig :: K.LogEnv -> [ClientConfig] -> IO [ClientConfig]
validateConfig le c = do
  withSecretsConf <- P.mapM (mapSecrets le) c
  P.mapM (mapLoginToken le) withSecretsConf

-- Check if the Http exception contains an 401 status code.
is401 :: R.HttpException -> Bool
is401 (R.VanillaHttpException (HttpExceptionRequest _ (StatusCodeException res _)))
  = statusCode (responseStatus res) == 401
is401 _ = False

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
homeHandler :: ScottyT L.Text (AppMonad IO) ()
homeHandler =
  get "/" $ html "<h1> Azure Exporter </h1> <a href='/metrics'>Metrics</a>"

metricsHandler :: ScottyT L.Text (AppMonad IO) ()
metricsHandler = get "/metrics" $ do
  conf <- appM $ gets clientConfig

  appM $ logMs
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

  appM $ logMs
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
      appM $ logMs K.WarningS "There are expired tokens. Trying to re-login."

      le      <- appM K.getLogEnv
      newConf <- liftIO $ P.mapM (mapLoginToken le) conf
      appM $ modify $ \c -> c { clientConfig = newConf }

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
    -> ActionT L.Text (AppMonad IO) ()
  sendResponse e v = if null e
    then text $ L.intercalate "\n" $ renderMetrics v
    else do
      let firstErr = head e

      appM $ logMs K.WarningS (renderError firstErr)

      status status500

      case firstErr of
        R.VanillaHttpException (HttpExceptionRequest _ (StatusCodeException _ b))
          -> json (A.decode (LBS.fromStrict b) :: Maybe ExporterError)
        R.JsonHttpException jsonError -> json $ AzureError
          { azError = MetricsApiError
            { code    = "JsonHttpException"
            , message = pack jsonError
            }
          }
        _ -> json $ AzureError
          { azError = MetricsApiError
            { code    = "UnknownError"
            , message = "Something went wrong. Check the logs for more details."
            }
          }

renderError :: R.HttpException -> Text
renderError (R.VanillaHttpException (HttpExceptionRequest _ content)) =
  pack $ show content
renderError (R.VanillaHttpException (InvalidUrlException url reason)) =
  pack $ "Failed to perform invalid request: " <> url <> " (" <> reason <> ")"
renderError (R.JsonHttpException e) = pack e

setupLogger :: EnvType -> Wai.Middleware
setupLogger Prod = logStdout
setupLogger Dev  = logStdoutDev

setupLogEnv :: EnvType -> K.Severity -> IO K.LogEnv
setupLogEnv Prod = prodEnv
setupLogEnv Dev  = devEnv

runIO :: TVar AppState -> AppMonad IO a -> IO a
runIO state m = runReaderT (runAppM m) state

data CliOpts = CliOpts
    { optFile            :: String
    , optListDefinitions :: Bool
    , optLogLevel        :: String
    }

cliOpts :: Opt.Parser CliOpts
cliOpts =
  CliOpts
    <$> Opt.strOption
          (  Opt.long "config-file"
          <> Opt.short 'f'
          <> Opt.help "Exporter settings"
          <> Opt.metavar "FILE"
          <> Opt.showDefault
          <> Opt.value "config.yaml"
          )
    <*> Opt.switch
          (  Opt.long "list-definitions"
          <> Opt.help "List the metric definitions for all resources"
          <> Opt.short 'd'
          )
    <*> Opt.strOption
          (  Opt.long "log-level"
          <> Opt.help "Specify the log severity level"
          <> Opt.metavar "LEVEL"
          <> Opt.showDefault
          <> Opt.value "info"
          <> Opt.short 'l'
          )

showListDefinitions :: K.LogEnv -> [ClientConfig] -> IO ()
showListDefinitions logenv c = do
  K.runKatipT logenv $ logMs K.InfoS "Fetching metric definitions"
  P.mapM_ fetchDefinitions c

fetchDefinitions :: ClientConfig -> IO ()
fetchDefinitions conf = do
  let subid = subscriptionId conf
  let tkn   = fromMaybe "" (loginToken conf)
  results <- liftIO $ P.mapM
    (runExceptT . runHttpM . getMetricDefinitions subid tkn)
    (resources conf)

  let (errs, metricResults) = partitionEithers results

  if null errs then mapM_ prettyPrintDefinitions metricResults else print errs

prettyPrintDefinitions :: AzureMetricDefinitionResponse -> IO ()
prettyPrintDefinitions r = do
  let values = defValue r

  if null values
    then return ()
    else do
      let resourceId = head $ map mdResourceId values
      let names      = map (nameValue . mdName) values

      LBS.putStrLn $ encodePretty $ A.object [resourceId .= names]

opts :: Opt.ParserInfo CliOpts
opts = Opt.info
  (cliOpts <**> Opt.helper)
  (  Opt.fullDesc
  <> Opt.progDesc
       "Web service that retrieves metrics from Azure and exports them for Prometheus."
  <> Opt.header ("Azure Exporter :: v" <> showVersion version)
  )

main :: IO ()
main = do
  parsedOpts <- Opt.execParser opts
  port       <- fromInteger <$> readListenPort 3000 "PORT"
  runEnv     <- readRunEnv "ENV"
  confFile   <- loadConfigFile (optFile parsedOpts)
  either die (runCommands parsedOpts runEnv port) confFile

data EnvType = Dev | Prod deriving (Show, Eq)

runCommands
  :: CliOpts
     -- ^ CLI configuration.
  -> EnvType
     -- ^ Runtime environment (dev or production).
  -> Int
     -- ^ Port to listen to.
  -> [ClientConfig]
     -- ^ Client configuration per Azure subscription.
  -> IO ()
runCommands parsedOpts runEnv port initialConf = do
  logenv <- setupLogEnv
    runEnv
    (fromMaybe K.InfoS (K.textToSeverity $ pack $ optLogLevel parsedOpts))
  validConf     <- validateConfig logenv initialConf
  initialConfig <- newTVarIO
    (AppState {clientConfig = validConf, logEnv = logenv})

  if optListDefinitions parsedOpts
    then showListDefinitions logenv validConf
    else scottyT port (runIO initialConfig) (app runEnv)

app :: EnvType -> ScottyT L.Text (AppMonad IO) ()
app runEnv = do
  -- Setup middlewares.
  middleware (setupLogger runEnv)

  -- Define routes.
  homeHandler
  metricsHandler
