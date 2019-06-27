{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

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

logMs :: Katip m => Severity -> Text -> m ()
logMs sev msg =
  logMsg (Namespace {unNamespace = []}) sev (LogStr {unLogStr = fromText msg})

devEnv :: Severity -> IO LogEnv
devEnv sev =
  mkLogEnv writePairLog (Environment {getEnvironment = "dev"}) sev V3

prodEnv :: Severity -> IO LogEnv
prodEnv sev = mkLogEnv writeLog (Environment {getEnvironment = "prod"}) sev V3

mkLogEnv :: WriteLogFn -> Environment -> Severity -> Verbosity -> IO LogEnv
mkLogEnv fn env sev verb = do
  logEnv <- initLogEnv (Namespace [""]) env
  scribe <- mkScribe fn stdout sev verb
  registerScribe "azure-exporter" scribe defaultScribeSettings logEnv

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

type WriteLogFn = forall a. MVar() -> Handle -> Verbosity -> Item a -> IO ()

mkScribe :: WriteLogFn -> Handle -> Severity -> Verbosity -> IO Scribe
mkScribe wfn h sev verb = do
  hSetBuffering h LineBuffering
  lock <- newMVar ()
  pure $ Scribe (wfn lock h verb) (hFlush h) (permitItem sev)
