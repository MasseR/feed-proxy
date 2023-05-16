{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module MyLib (defaultMain) where


import Control.Monad (when, (<=<))
import Control.Monad.FeedProxy
import Data.Environment

import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Except


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

import Data.Map (Map)
import qualified Data.Map as M

import Control.Lens


import Control.Exception (SomeException, displayException, try)
import Data.Either (fromRight)
import Data.Feed.Render (render)
import Database (runMigrations)
import FeedProxy
       (Configuration, autoilevaMotoristi, erlware, evalConfiguration, poloinen)
import FeedProxy.Feed (Feed'(feedSlug), feedToAtom)
import qualified Katip as K
import Network.Wai.Metrics (metrics, registerWaiMetrics)
import Servant.Metrics.Prometheus
import System.Metrics
       (Store, newStore, registerGcMetrics)
import UnliftIO (MonadIO)
import UnliftIO.Async (mapConcurrently_)

data Atom

instance Accept Atom where
  contentType _ = "application" // "atom+xml"

instance MimeRender Atom Feed where
  mimeRender _ = fromRight "" . render

data Routes route
  = Routes { _getFeed :: route :- "feed" :> Capture "slug" Text :> Get '[Atom] Feed
           , _getFeeds :: route :- "feed" :> Get '[JSON] [Text]
           , _getMetrics :: route :- MetricsRoute
           }


deriving stock instance Generic (Routes route)

newtype Feeds = Feeds { getFeeds :: Map Text Configuration  }

instance Semigroup Feeds where
  Feeds a <> Feeds b = Feeds $ M.union a b

instance Monoid Feeds where
  mempty = Feeds M.empty

internalFeeds :: Feeds
internalFeeds = Feeds $ foldMap (\p -> M.singleton (feedSlug p) p)
  [ poloinen
  , autoilevaMotoristi
  , erlware
  ]

server :: Trace FeedProxyM Fetch -> Store -> Feeds -> Routes (AsServerT FeedProxyM)
server tracer store feeds =
  Routes { _getFeed = maybe (throwM err404) pure <=< runFeed tracer feeds
         , _getFeeds = pure $ M.keys $ getFeeds feeds
         , _getMetrics = metricsServerT store
  }

data Fetch = Fetch String ()

runFeed :: Trace FeedProxyM Fetch -> Feeds -> Text -> FeedProxyM (Maybe Feed)
runFeed _tracer feeds name = do
  -- Find the configuration from the map of feeds, if the configuration exists,
  -- evaluate it and convert to an atom feed
  case M.lookup name (getFeeds feeds) of
    Nothing -> pure Nothing
    Just config -> Just . feedToAtom <$> evalConfiguration config

app :: Trace FeedProxyM Fetch -> Store -> Environment -> Feeds -> Application
app tracer store env feeds = genericServeT nt (server tracer store feeds)
  where
    nt :: FeedProxyM a -> Handler a
    nt = Handler . ExceptT . try @ServerError . runFeedProxy env

formatTrace :: Fetch -> K.LogStr
formatTrace = \case
  Fetch url () -> "Fetching '" <> K.ls url <> "'"

logTrace :: (K.KatipContext m) => Trace m K.LogStr
logTrace = Trace (K.logFM K.InfoS)

data FetchMetric = FetchMetric

ekgTrace :: MonadIO m => FetchMetric -> Trace m Fetch
ekgTrace FetchMetric{} = Trace $ \case
  Fetch _ () -> pure ()

defaultMain :: Int -> Environment -> Feeds -> IO ()
defaultMain port env feeds = do
  runMigrations (environmentConnection env)
  store <- newStore
  waiMetrics <- registerWaiMetrics store
  registerGcMetrics store
  -- This fetch metric thing is a bit of a dead code for now after
  -- refactoring.
  -- TODO: Restore metrics
  let fetchMetrics = FetchMetric
  let tracer = ekgTrace fetchMetrics <> contramap formatTrace logTrace
  mapConcurrently_ id
    [ runSettings settings (metrics waiMetrics $ app tracer store env (internalFeeds <> feeds))
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
