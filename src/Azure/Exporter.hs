{-# LANGUAGE OverloadedStrings #-}

module Azure.Exporter
  ( renderMetrics
  )
where

import qualified Data.Text                     as T
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )
import           Data.Text.Lazy.Builder         ( fromText
                                                , fromString
                                                , toLazyText
                                                , singleton
                                                , Builder
                                                )
import qualified Data.Text.Lazy                as L

import           Azure.Client

joinB :: Builder -> [Builder] -> Builder
joinB _ []       = singleton ' '
joinB _ [x     ] = x
joinB s (x : xs) = x <> s <> joinB s xs

maybeElem :: Int -> [a] -> Maybe a
maybeElem n l = if length l >= n then Just (l !! n) else Nothing

unique :: Eq a => [a] -> [a]
unique []       = []
unique (x : xs) = x : unique (filter (x /=) xs)

-- Convert the Azure response to the exposition format.
renderMetrics
  :: [[MetricValueResponse]]
  -- ^ A list of metric values for each subscription.
  -> [L.Text]
  -- ^ Metrics in the Prometheus exposition format.
renderMetrics = unique . map toLazyText . concatMap render'
 where
  render' :: [MetricValueResponse] -> [Builder]
  render' = concatMap renderValueResponse

  renderValueResponse :: MetricValueResponse -> [Builder]
  renderValueResponse v = concatMap renderMetricValue (metricValue v)

renderMetricValue :: MetricValue -> [Builder]
renderMetricValue v = concatMap renderSingleMetric aggregationPairs
 where
  id'       = metricId v
  name'     = nameValue $ metricName v
  desc'     = nameLocalizedValue $ metricName v
  elemValue = case maybeElem 0 (metricTimeseries v) of
    Nothing -> Nothing
    Just ts -> maybeElem 0 (timeseriesData ts)

  renderSingleMetric (ext, value_) =
    metricSpec (name' <> "_" <> ext) desc' value_ id'

  aggregationPairs = catMaybePairs $ zip
    ["Count", "Total", "Average", "Minimum", "Maximum"]
    [countV, totalV, avgV, minV, maxV]

  catMaybePairs ls = [ (x, y) | (x, Just y) <- ls ]

  countV = dataCount =<< elemValue
  totalV = dataTotal =<< elemValue
  avgV   = dataAverage =<< elemValue
  minV   = dataMinimum =<< elemValue
  maxV   = dataMaximum =<< elemValue

typeLine :: Builder -> Builder
typeLine n = hashTag <> space <> typeStr <> space <> n <> space <> gaugeType

helpLine :: Builder -> Builder -> Builder
helpLine n desc = hashTag <> space <> helpStr <> space <> n <> space <> desc

metricSpec :: T.Text -> T.Text -> Double -> T.Text -> [Builder]
metricSpec mName mDesc mValue resId = [help', type', metricLine name' mValue resId]
 where
  name' =
    fromText $ (T.toLower . T.replace " " "_" . T.replace "/" "_per_") mName
  help' = helpLine name' (fromText mDesc)
  type' = typeLine name'

metricLine
  :: Builder
  -- ^ The initial name of the exported metric.
  -> Double
  -- ^ The current value of the metric.
  -> T.Text
  -- ^ The extracted resource ID.
  -> Builder
metricLine mName mValue resId =
  mName <> metricLabels <> space <> (fromString . show) mValue
 where
  metricLabels :: Builder
  metricLabels = leftBracket <> renderedLabels <> rightBracket

  renderedLabels :: Builder
  renderedLabels =
    joinB comma (map stringify (M.toList (extractResourceLabels resId)))

  stringify :: (Builder, Builder) -> Builder
  stringify (k, v) = k <> equal <> doubleQuote <> v <> doubleQuote

equal :: Builder
equal = fromText "="

comma :: Builder
comma = fromText ","

doubleQuote :: Builder
doubleQuote = fromText "\""

leftBracket :: Builder
leftBracket = fromText "{"

rightBracket :: Builder
rightBracket = fromText "}"

space :: Builder
space = fromText " "

gaugeType :: Builder
gaugeType = fromText "gauge"

helpStr :: Builder
helpStr = fromText "HELP"

typeStr :: Builder
typeStr = fromText "TYPE"

hashTag :: Builder
hashTag = fromText "#"

extractResourceLabels :: T.Text -> M.Map Builder Builder
extractResourceLabels resourceId = M.fromList
  (resourceGroup <> resourceName <> subResourceName)
 where
  allItems :: [Builder]
  allItems = map fromText (T.splitOn "/" resourceId)

  toList :: Builder -> Maybe Builder -> [(Builder, Builder)]
  toList keyName expr = catMaybes [(,) keyName <$> expr]

  resourceGroup :: [(Builder, Builder)]
  resourceGroup = toList "resource_group" (maybeElem 4 allItems)

  resourceName :: [(Builder, Builder)]
  resourceName = toList "resource_name" (maybeElem 8 allItems)

  subResourceName :: [(Builder, Builder)]
  subResourceName = toList
    "sub_resource_name"
    (if length allItems > 13 then maybeElem 10 allItems else Nothing)
