{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Azure.Config
  ( ClientConfig(..)
  , AzureResource(..)
  , loadConfigFile
  , mapSecrets
  )
where

import qualified Data.ByteString.Char8         as B
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Y
import           GHC.Generics
import           System.Environment            as E
import           Katip                         as K

import           Logger

data AzureResource = AzureResource
  { name          :: T.Text
  -- ^ The name of the resource.
  , metrics :: [T.Text]
  -- ^ The metrics to scrape from that resource.
  } deriving (Eq, Read, Show, Generic)

instance Y.FromJSON AzureResource

instance Y.ToJSON AzureResource

data ClientConfig = ClientConfig
  { resources          :: [AzureResource]
  -- ^ The resources for which we retrieve metrics.
  , subscriptionId     :: T.Text
  -- ^ The Azure Subscripiton ID.
  , tenantId           :: T.Text
  -- ^ The Azure Tenant ID
  , clientId           :: T.Text
  -- ^ The Azure Client ID.
  , clientSecretEnvVar :: T.Text
  -- ^ The environment variable that will hold the actual client secret.
  , clientSecret       :: Maybe T.Text
  -- ^ The secret token used for login. It will remain `Nothing` until its
  -- value is retrieved from the environment variable.
  , loginToken         :: Maybe T.Text
  -- ^ The authentication token used for all requests to Azure. It will remain `Nothing`
  -- until it's obtained.
  } deriving (Eq, Read, Show, Generic)

instance Y.FromJSON ClientConfig

instance Y.ToJSON ClientConfig

mapSecrets :: K.LogEnv -> ClientConfig -> IO ClientConfig
mapSecrets logenv conf = do
  val <- lookupEnv varName
  case val of
    Just a  -> return $ updateConfig $ Just $ T.pack a
    Nothing -> do
      K.runKatipT logenv $ logMs
        K.ErrorS
        (T.pack $ "Environment variable " <> varName <> " is empty.")
      return conf
 where
  varName = T.unpack $ clientSecretEnvVar conf
  updateConfig secretValue = conf { clientSecret = secretValue }

loadConfigFile :: FilePath -> IO (Either String [ClientConfig])
loadConfigFile f = do
  content <- B.readFile f
  let parsedContent =
        Y.decodeEither' content :: Either Y.ParseException [ClientConfig]
  case parsedContent of
    Left  err -> return $ Left $ Y.prettyPrintParseException err
    Right a   -> return $ Right a
