{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
module MyLib (defaultMain) where

import Data.Feed.Erlware

import Control.Applicative (empty)
import Control.Monad (when, (<=<))
import Control.Monad.FeedProxy
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Environment

import Control.Monad.Catch (throwM, catch)
import Control.Monad.Trans.Except

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit (Response, responseBody)
import qualified Text.HTML.DOM as DOM

import Data.Feed.Render

import Network.HTTP.Media.MediaType ((//))
import Network.Wai (Request)
import Network.Wai.Handler.Warp
       ( defaultSettings
       , defaultShouldDisplayException
       , runSettings
       , setOnException
       , setPort
       )
import Servant
import Servant.API.Generic
import Servant.Server.Generic

import Data.Text (Text)

import Data.Trace

import Text.Atom.Feed (Feed)

import Data.Feed.Parser (FeedParser(origin, slug))
import Data.Map (Map)
import qualified Data.Map as M
import Text.XML (Element)

import Control.Lens
import Text.XML.Lens (root)

import Control.Feed.Fetch (FetchTrace(..), getFeed, cacheRefresher)

import qualified Katip as K
import Data.Either (fromRight)
import Data.Feed.Autotie (autotie)
import UnliftIO.Async (forConcurrently_, mapConcurrently_)
import Control.Exception (try)
import UnliftIO (isAsyncException, liftIO, MonadIO)
import Control.Exception (SomeException)
import Control.Exception (displayException)
import System.Metrics (newStore, registerGcMetrics, Sample, Store, sampleAll, Value (..), createCounter, createDistribution)
import Network.Wai.Metrics (registerWaiMetrics, metrics)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Text.Printf as Text
import qualified System.Metrics.Distribution as Stats
import qualified System.Metrics.Counter as Counter

data Atom

instance Accept Atom where
  contentType _ = "application" // "atom+xml"

instance MimeRender Atom Feed where
  mimeRender _ = fromRight "" . render

data Routes route
  = Routes { _getFeed :: route :- "feed" :> Capture "slug" Text :> Get '[Atom] Feed
           , _getFeeds :: route :- "feed" :> Get '[JSON] [Text]
           , _getMetrics :: route :- "metrics" :> Get '[PlainText] Metrics
           }

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

deriving stock instance Generic (Routes route)

feeds :: Map Text (FeedParser Element)
feeds = foldMap (\p -> M.singleton (slug p) p)
  [ autotie "autoileva-motoristi" "https://www.autotie.fi/tien-sivusta/sahkoautoileva-motoristi"
  , autotie "poloinen" "https://www.autotie.fi/tien-sivusta/poloinen"
  , erlware
  ]

server :: Trace FeedProxyM Fetch -> Store -> Routes (AsServerT FeedProxyM)
server tracer store =
  Routes { _getFeed = maybe (throwM err404) pure <=< runFeed tracer
         , _getFeeds = pure $ M.keys feeds
         , _getMetrics = liftIO $ Metrics <$> sampleAll store
  }

data Fetch = Fetch String FetchTrace

runFeed :: Trace FeedProxyM Fetch -> Text -> FeedProxyM (Maybe Feed)
runFeed tracer name = runMaybeT $ do
  parser <- toMaybeT (M.lookup name feeds)
  let url = origin parser
  MaybeT (getFeed (contramap (Fetch url) tracer) (contramap toElement parser))
  where
    toMaybeT = maybe empty pure

toElement :: Response ByteString -> Element
toElement lbs = DOM.parseLBS (responseBody lbs) ^. root

app :: Trace FeedProxyM Fetch -> Store -> Environment -> Application
app tracer store env = genericServeT nt (server tracer store)
  where
    nt :: FeedProxyM a -> Handler a
    nt = Handler . ExceptT . try @ServerError . runFeedProxy env

formatTrace :: Fetch -> K.LogStr
formatTrace = \case
  Fetch url (FetchNew _) -> "Fetching '" <> K.ls url <> "'"
  Fetch url Hit -> "Cache hit for '" <> K.ls url <> "'"
  Fetch url Miss -> "Cache miss for '" <> K.ls url <> "'"
  Fetch url Scheduled -> "Scheduled fetch for '" <> K.ls url <> "'"

logTrace :: (K.KatipContext m) => Trace m K.LogStr
logTrace = Trace (K.logFM K.InfoS)

data FetchMetric = FetchMetric
  { fetchHits :: Counter.Counter
  , fetchMisses :: Counter.Counter
  , fetchDuration :: Stats.Distribution
  }

ekgTrace :: MonadIO m => FetchMetric -> Trace m Fetch
ekgTrace FetchMetric{..} = Trace $ \case
  Fetch _ Hit -> liftIO $ Counter.inc fetchHits
  Fetch _ Miss -> liftIO $ Counter.inc fetchMisses
  Fetch _ (FetchNew n) -> liftIO $ Stats.add fetchDuration n
  _ -> pure ()

scheduleWorker :: Trace FeedProxyM Fetch -> Environment -> IO ()
scheduleWorker tracer env =
  runFeedProxy env $ forConcurrently_ feeds $ \feed -> do
    let url = origin feed
    (cacheRefresher (contramap (Fetch url) tracer) . contramap toElement $ feed) `catch` errorHandler
  where
    errorHandler :: SomeException -> FeedProxyM ()
    errorHandler e | isAsyncException e = throwM e
                   | otherwise = K.logFM K.ErrorS $ K.ls $ show e

defaultMain :: Int -> Environment -> IO ()
defaultMain port env = do
  store <- newStore
  waiMetrics <- registerWaiMetrics store
  registerGcMetrics store
  fetchMetrics <- FetchMetric <$>
    createCounter "cache.hit" store <*>
    createCounter "cache.miss" store <*>
    createDistribution "download" store
  let tracer = (ekgTrace fetchMetrics) <> contramap formatTrace logTrace -- <> contramap (formatTrace . Fetch url) logTrace
  mapConcurrently_ id
    [ scheduleWorker tracer env
    , runSettings settings (metrics waiMetrics $ app tracer store env)
    ]
  where
    settings =
      defaultSettings &
      setPort port &
      setOnException onException
    onException :: Maybe Request -> SomeException -> IO ()
    onException _req exc =
      when (defaultShouldDisplayException exc) $
        runFeedProxy env $ K.logFM K.ErrorS $ K.ls (displayException exc)
