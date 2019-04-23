{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Logger
  ( devEnv
  , prodEnv
  , logMs
  )
where

import           Katip
import           Katip.Format.Time              ( formatAsIso8601 )
import           Control.Concurrent
import           Control.Exception              ( bracket_ )
import           Data.Aeson
import           Data.Aeson.Text
import qualified Data.HashMap.Strict           as HM
import           Data.Text                      ( Text )
import           Data.Text.Internal.Builder
import qualified Data.Text.Lazy                as LT
import           Data.Text.Lazy.IO             as T
import           System.IO

logMs :: Katip m => Severity -> Text -> m ()
logMs sev msg = logMsg (Namespace { unNamespace = [] })
                       sev
                       (LogStr { unLogStr = fromText msg })

devEnv :: IO LogEnv
devEnv = mkLogEnv "" (Environment { getEnvironment = "dev" }) DebugS V3

prodEnv :: IO LogEnv
prodEnv = mkLogEnv "" (Environment { getEnvironment = "prod" }) WarningS V1

mkLogEnv :: Text -> Environment -> Severity -> Verbosity -> IO LogEnv
mkLogEnv appName env sev verb = do
  le <- initLogEnv (Namespace [appName]) env
  lh <- mkLogScribe stdout sev verb
  registerScribe "azure-exporter" lh defaultScribeSettings le

formatItem :: Verbosity -> Item a -> HM.HashMap Text Value
formatItem _ Item {..} = HM.fromList
  [ ("ts"   , String $ formatAsIso8601 _itemTime)
  , ("env"  , String $ getEnvironment _itemEnv)
  , ("level", String $ renderSeverity _itemSeverity)
  , ("msg"  , String $ LT.toStrict $ toLazyText $ unLogStr _itemMessage)
  ]

writeLog :: MVar () -> Handle -> Verbosity -> Item a -> IO ()
writeLog lock h verb item =
  bracket_ (takeMVar lock) (putMVar lock ())
    $ T.hPutStrLn h
    $ encodeToLazyText
    $ Object
    $ formatItem verb item

mkLogScribe :: Handle -> Severity -> Verbosity -> IO Scribe
mkLogScribe h sev verb = do
  hSetBuffering h LineBuffering
  lock <- newMVar ()
  pure $ Scribe (writeLog lock h verb) (hFlush h) (permitItem sev)
