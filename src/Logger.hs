{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Logger
  ( devEnv
  , prodEnv
  , logMs
  )
where

import           Prelude                 hiding ( unwords )
import           Katip
import           Katip.Format.Time              ( formatAsIso8601 )
import           Control.Concurrent
import           Control.Exception              ( bracket_ )
import           Data.Aeson
import           Data.Aeson.Text
import qualified Data.HashMap.Strict           as HM
import           Data.Text                      ( Text
                                                , unwords
                                                )
import           Data.Text.Internal.Builder
import qualified Data.Text.Lazy                as LT
import           Data.Text.Lazy.IO             as T
import           System.IO

-- TODO: Remove duplication.

logMs :: Katip m => Severity -> Text -> m ()
logMs sev msg =
  logMsg (Namespace {unNamespace = []}) sev (LogStr {unLogStr = fromText msg})

devEnv :: Severity -> IO LogEnv
devEnv sev = devLogEnv "" (Environment {getEnvironment = "dev"}) sev V3

prodEnv :: Severity -> IO LogEnv
prodEnv sev = prodLogEnv "" (Environment {getEnvironment = "prod"}) sev V3

devLogEnv :: Text -> Environment -> Severity -> Verbosity -> IO LogEnv
devLogEnv appName env sev verb = do
  le <- initLogEnv (Namespace [appName]) env
  lh <- mkLogScribe stdout sev verb
  registerScribe "azure-exporter" lh defaultScribeSettings le

prodLogEnv :: Text -> Environment -> Severity -> Verbosity -> IO LogEnv
prodLogEnv appName env sev verb = do
  le <- initLogEnv (Namespace [appName]) env
  lh <- mkJsonLogScribe stdout sev verb
  registerScribe "azure-exporter" lh defaultScribeSettings le

formatToJson :: Verbosity -> Item a -> HM.HashMap Text Value
formatToJson _ Item {..} = HM.fromList
  [ ("ts"   , String $ formatAsIso8601 _itemTime)
  , ("env"  , String $ getEnvironment _itemEnv)
  , ("level", String $ renderSeverity _itemSeverity)
  , ("msg"  , String $ LT.toStrict $ toLazyText $ unLogStr _itemMessage)
  ]

formatToPairs :: Verbosity -> Item a -> Text
formatToPairs _ Item {..} = unwords $ map
  (\(k, v) -> k <> "=" <> v)
  [ ("ts"   , formatAsIso8601 _itemTime)
  , ("level", renderSeverity _itemSeverity)
  , ("env"  , getEnvironment _itemEnv)
  , ("msg"  , LT.toStrict $ toLazyText $ unLogStr _itemMessage)
  ]

writeLog :: MVar () -> Handle -> Verbosity -> Item a -> IO ()
writeLog lock h verb item =
  bracket_ (takeMVar lock) (putMVar lock ())
    $ T.hPutStrLn h
    $ encodeToLazyText
    $ formatToJson verb item

writePairLog :: MVar () -> Handle -> Verbosity -> Item a -> IO ()
writePairLog lock h verb item =
  bracket_ (takeMVar lock) (putMVar lock ())
    $ T.hPutStrLn h
    $ encodeToLazyText
    $ formatToPairs verb item

mkLogScribe :: Handle -> Severity -> Verbosity -> IO Scribe
mkLogScribe h sev verb = do
  hSetBuffering h LineBuffering
  lock <- newMVar ()
  pure $ Scribe (writePairLog lock h verb) (hFlush h) (permitItem sev)

mkJsonLogScribe :: Handle -> Severity -> Verbosity -> IO Scribe
mkJsonLogScribe h sev verb = do
  hSetBuffering h LineBuffering
  lock <- newMVar ()
  pure $ Scribe (writeLog lock h verb) (hFlush h) (permitItem sev)
