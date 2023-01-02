{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Servant.Metrics.Prometheus where

import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Servant (Server, ServerT)
import Servant.API
import System.Metrics (Sample, Store, Value(..), sampleAll)
import qualified System.Metrics.Distribution as Stats
import qualified Text.Printf as Text

newtype Metrics = Metrics Sample

instance MimeRender PlainText Metrics where
  mimeRender _ (Metrics sample) = LB.fromStrict . TE.encodeUtf8 . T.unlines . map (uncurry format) . HM.toList $ sample
    where
      formatKey :: Text -> Text
      formatKey k = "feed_proxy_" <> T.replace "." "_" k
      format :: Text -> Value -> Text
      format key = \case
        Counter n -> T.pack $ Text.printf "%s %d" (formatKey key) n
        Gauge n -> T.pack $ Text.printf "%s %d" (formatKey key) n
        Label n -> T.pack $ Text.printf "%s{label=\"%s\"} 1.0" (formatKey key) n
        Distribution d ->
          let k = formatKey key
          in T.pack $ Text.printf "%s_sum %f\n%s_count %d" k (Stats.sum d) k (Stats.count d)

type MetricsRoute = "metrics" :> Get '[PlainText] Metrics

metricsServerT :: MonadIO m => Store -> ServerT MetricsRoute m
metricsServerT store = liftIO (Metrics <$> sampleAll store)

metricsServer :: Store -> Server MetricsRoute
metricsServer store = liftIO (Metrics <$> sampleAll store)
