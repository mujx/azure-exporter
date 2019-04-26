{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}

module Azure.Client
  ( MetricValueResponse(..)
  , LoginForm(..)
  , AzureError(..)
  , HttpM(..)
  , MetricsApiError(..)
  , MetricValue(..)
  , TimeseriesData(..)
  , MetricTimeseries(..)
  , MetricDefinitionResponse(..)
  , AzureMetricDefinitionResponse(..)
  , MetricName(..)
  , getAccessToken
  , mapLoginToken
  , getMetricValue
  , getMetricDefinitions
  )
where

import qualified Katip                         as K
import           Control.Monad.Except
import qualified Control.Monad.Parallel        as P
import           Azure.Config
import           Control.Exception              ( try )
import           Data.Aeson
import           Data.Aeson.Casing              ( snakeCase )
import           Data.Aeson.Types               ( Options(..)
                                                , defaultOptions
                                                )
import qualified Data.ByteString               as BS
import           Data.Char                     as C
import           Data.List                      ( intercalate )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text
                                                , stripPrefix
                                                , unpack
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           GHC.Generics
import qualified Network.HTTP.Req              as R
import           Network.HTTP.Req               ( (=:)
                                                , (/:)
                                                )

import           Logger

apiVersion :: Text
apiVersion = "2018-01-01"

metricDefiniotionsEndpoint
  :: Text
     -- ^ The Azure Subscription ID.
  -> Text
     -- ^ The resource name as a URL path.
  -> R.Url 'R.Https
     -- ^ The full parsed HTTPS URL.
metricDefiniotionsEndpoint subid resource =
  R.https "management.azure.com"
    /: "subscriptions"
    /: subid
    /: rmLeadingSlash resource
    /: "providers"
    /: "microsoft.insights"
    /: "metricDefinitions"

metricValueEndpoint
  :: Text
     -- ^ The Azure Subscription ID.
  -> Text
     -- ^ The resource name as a URL path.
  -> R.Url 'R.Https
     -- ^ The full parsed HTTPS URL.
metricValueEndpoint subid resource =
  R.https "management.azure.com"
    /: "subscriptions"
    /: subid
    /: rmLeadingSlash resource
    /: "providers"
    /: "microsoft.insights"
    /: "metrics"

rmLeadingSlash :: Text -> Text
rmLeadingSlash (stripPrefix "/" -> Just xs) = xs
rmLeadingSlash x                            = x

loginUrl :: Text -> R.Url 'R.Https
loginUrl tid =
  R.https "login.microsoftonline.com" /: tid /: "oauth2" /: "token"

-- Default parse options to convert snakeCase json fields to camelCase record fields.
noCamelCaseOptions :: Options
noCamelCaseOptions = defaultOptions { fieldLabelModifier = snakeCase }

noPrefix :: String -> String
noPrefix = firstToLower . dropWhile C.isLower
 where
  firstToLower []       = []
  firstToLower (x : xs) = C.toLower x : xs

noPrefixOptions :: Options
noPrefixOptions = defaultOptions { fieldLabelModifier = noPrefix }

newtype AzureMetricDefinitionResponse = AzureMetricDefinitionResponse
  { defValue :: [MetricDefinitionResponse]
  } deriving (Show, Generic)

instance FromJSON AzureMetricDefinitionResponse where
  parseJSON = genericParseJSON noPrefixOptions

data MetricDefinitionResponse = MetricDefinitionResponse
  { mdId                     :: Text
  , mdIsDimensionRequired    :: Bool
  , mdPrimaryAggregationType :: Text
  , mdResourceId             :: Text
  , mdUnit                   :: Text
  , mdName                   :: MetricName
  } deriving (Show, Generic)

instance FromJSON MetricDefinitionResponse where
  parseJSON = genericParseJSON noPrefixOptions

data LoginResponse = LoginResponse
  { accessToken  :: Text
  , extExpiresIn :: Text
  , notBefore    :: Text
  , expiresOn    :: Text
  } deriving (Show, Generic)

instance FromJSON LoginResponse where
  parseJSON = genericParseJSON noCamelCaseOptions

newtype AzureError = AzureError
    { azError :: MetricsApiError
    } deriving (Show, Eq, Generic)

instance FromJSON AzureError where
  parseJSON = genericParseJSON noPrefixOptions

instance ToJSON AzureError where
  toJSON = genericToJSON noPrefixOptions

data ApiError = ApiError
  { err              :: Text
  , errorDescription :: Text
  } deriving (Show, Generic)

instance FromJSON ApiError where
  parseJSON = genericParseJSON noCamelCaseOptions

data LoginForm = LoginForm
  { formGrantType    :: Text
  , formResource     :: Text
  , formClientId     :: Text
  , formClientSecret :: Text
  } deriving (Show, Generic)

getAccessToken :: Text -> LoginForm -> IO LoginResponse
getAccessToken tid loginForm = R.runReq R.defaultHttpConfig $ do
  let params =
        ("grant_type" =: formGrantType loginForm)
          <> ("resource" =: formResource loginForm)
          <> ("client_id" =: formClientId loginForm)
          <> ("client_secret" =: formClientSecret loginForm)
  response <- R.req R.POST
                    (loginUrl tid)
                    (R.ReqBodyUrlEnc params)
                    R.jsonResponse
                    mempty
  return $ R.responseBody response

--
-- Types from the metrics endpoint.
--
data TimeseriesData = TimeseriesData
  { dataTimeStamp :: Text
  , dataTotal     :: Maybe Double
  , dataAverage   :: Maybe Double
  , dataMinimum   :: Maybe Double
  , dataMaximum   :: Maybe Double
  } deriving (Show, Eq, Generic)

instance FromJSON TimeseriesData where
  parseJSON = genericParseJSON noPrefixOptions

newtype MetricTimeseries = MetricTimeseries
  { timeseriesData :: [TimeseriesData]
  } deriving (Show, Eq, Generic)

instance FromJSON MetricTimeseries where
  parseJSON = genericParseJSON noPrefixOptions

data MetricName = MetricName
  { nameLocalizedValue :: Text
  , nameValue          :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON MetricName where
  parseJSON = genericParseJSON noPrefixOptions

data MetricValue = MetricValue
  { metricTimeseries :: [MetricTimeseries]
  , metricId         :: Text
  , metricName       :: MetricName
  , metricType       :: Text
  , metricUnit       :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON MetricValue where
  parseJSON = genericParseJSON noPrefixOptions

data MetricsApiError = MetricsApiError
  { code    :: Text
  , message :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON MetricsApiError

instance ToJSON MetricsApiError where

data MetricValueResponse = MetricValueResponse
  { metricValue :: [MetricValue]
  , metricError :: Maybe MetricsApiError
  } deriving (Show, Eq, Generic)

instance FromJSON MetricValueResponse where
  parseJSON = genericParseJSON noPrefixOptions

-- Monad to wrap around Req in order to handle its exceptions.
newtype HttpM a = HttpM
    { runHttpM :: ExceptT R.HttpException IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadError R.HttpException
           , P.MonadParallel
           )

instance R.MonadHttp HttpM where
  handleHttpException = throwError

getMetricDefinitions
  :: Text
    -- ^ The Azure Subscription ID.
  -> Text
    -- ^ The access token obtained by OAuth login.
  -> AzureResource
    -- ^ The resource & the metrics that will be queried.
  -> HttpM AzureMetricDefinitionResponse
    -- ^ Parsed response from the endpoint.
getMetricDefinitions subId tkn azResource = do
  let headers     = R.header "Authorization" (setToken tkn)
  let queryParams = "api-version" =: apiVersion
  let options     = headers <> queryParams

  r <- R.req R.GET endpoint R.NoReqBody R.jsonResponse options

  return (R.responseBody r :: AzureMetricDefinitionResponse)
 where
  endpoint :: R.Url 'R.Https
  endpoint = metricDefiniotionsEndpoint subId (name azResource)

  setToken :: Text -> BS.ByteString
  setToken token = encodeUtf8 $ "Bearer " <> token

getMetricValue
  :: Text
    -- ^ The Azure Subscription ID.
  -> Text
    -- ^ The access token obtained by OAuth login.
  -> AzureResource
    -- ^ The resource & the metrics that will be queried.
  -> HttpM MetricValueResponse
    -- ^ Parsed response from the endpoint.
getMetricValue subId tkn azResource = do
  let headers = R.header "Authorization" (setToken tkn)
  let queryParams =
        ("metricnames" =: joinMetrics azResource)
          <> ("aggregation" =: aggregations)
          <> ("api-version" =: apiVersion)
          <> ("timespan" =: threeMinTimespan)
  let options = headers <> queryParams

  r <- R.req R.GET endpoint R.NoReqBody R.jsonResponse options

  return (R.responseBody r :: MetricValueResponse)
 where
  endpoint :: R.Url 'R.Https
  endpoint = metricValueEndpoint subId (name azResource)

  setToken :: Text -> BS.ByteString
  setToken token = encodeUtf8 $ "Bearer " <> token

  aggregations :: Text
  aggregations = "Total,Average,Minimum,Maximum"

  threeMinTimespan :: Text
  threeMinTimespan = "PT3M"

  joinMetrics :: AzureResource -> String
  joinMetrics res = intercalate "," (map unpack $ metrics res)

newtype ClientID =
  ClientID Text

newtype ClientSecret =
  ClientSecret Text

mkLoginForm :: ClientID -> ClientSecret -> LoginForm
mkLoginForm (ClientID clientId_) (ClientSecret clientSecret_) = LoginForm
  { formGrantType    = "client_credentials"
  , formResource     = "https://management.azure.com"
  , formClientId     = clientId_
  , formClientSecret = clientSecret_
  }

mapLoginToken :: K.LogEnv -> ClientConfig -> IO ClientConfig
mapLoginToken logenv conf = case clientSecret conf of
  Just secret -> do
    K.runKatipT logenv $ logMs K.InfoS "Trying to login"

    res <- try (getAccessToken tenantId' loginForm)

    case res of
      Left (R.VanillaHttpException _) -> return conf
      Left (R.JsonHttpException _) -> return conf
      Right v -> return conf { loginToken = Just (accessToken v) }
   where
    loginForm = mkLoginForm (ClientID (clientId conf)) (ClientSecret secret)
    tenantId' = tenantId conf
  Nothing -> do
    K.runKatipT logenv $ logMs
      K.ErrorS
      (  "Client Secret is missing for Client ID ("
      <> clientId conf
      <> "). Login cannot be completed."
      )
    return conf
