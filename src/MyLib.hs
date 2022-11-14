{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
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
import UnliftIO (isAsyncException)
import Control.Exception (SomeException)
import Control.Exception (displayException)

data Atom

instance Accept Atom where
  contentType _ = "application" // "atom+xml"

instance MimeRender Atom Feed where
  mimeRender _ = fromRight "" . render

data Routes route
  = Routes { _getFeed :: route :- "feed" :> Capture "slug" Text :> Get '[Atom] Feed
           , _getFeeds :: route :- "feed" :> Get '[JSON] [Text]
           }

deriving stock instance Generic (Routes route)

feeds :: Map Text (FeedParser Element)
feeds = foldMap (\p -> M.singleton (slug p) p)
  [ autotie "autoileva-motoristi" "https://www.autotie.fi/tien-sivusta/sahkoautoileva-motoristi"
  , autotie "poloinen" "https://www.autotie.fi/tien-sivusta/poloinen"
  , erlware
  ]

server :: Routes (AsServerT FeedProxyM)
server =
  Routes { _getFeed = maybe (throwM err404) pure <=< runFeed
         , _getFeeds = pure $ M.keys feeds
  }

data Fetch = Fetch String FetchTrace

runFeed :: Text -> FeedProxyM (Maybe Feed)
runFeed name = runMaybeT $ do
  parser <- toMaybeT (M.lookup name feeds)
  let url = origin parser
  MaybeT (getFeed (contramap (formatTrace . Fetch url) logTrace) (contramap toElement parser))
  where
    toMaybeT = maybe empty pure
toElement :: Response ByteString -> Element
toElement lbs = DOM.parseLBS (responseBody lbs) ^. root

app :: Environment -> Application
app env = genericServeT nt server
  where
    nt :: FeedProxyM a -> Handler a
    nt = Handler . ExceptT . try @ServerError . runFeedProxy env

formatTrace :: Fetch -> K.LogStr
formatTrace = \case
  Fetch url FetchNew -> "Fetching '" <> K.ls url <> "'"
  Fetch url Hit -> "Cache hit for '" <> K.ls url <> "'"
  Fetch url Miss -> "Cache miss for '" <> K.ls url <> "'"
  Fetch url Scheduled -> "Scheduled fetch for '" <> K.ls url <> "'"

logTrace :: (K.KatipContext m) => Trace m K.LogStr
logTrace = Trace (K.logFM K.InfoS)

scheduleWorker :: Environment -> IO ()
scheduleWorker env =
  runFeedProxy env $ forConcurrently_ feeds $ \feed -> do
    let tracer = contramap (formatTrace . Fetch url) logTrace
        url = origin feed
    (cacheRefresher tracer . contramap toElement $ feed) `catch` errorHandler
  where
    errorHandler :: SomeException -> FeedProxyM ()
    errorHandler e | isAsyncException e = throwM e
                   | otherwise = K.logFM K.ErrorS $ K.ls $ show e

defaultMain :: Int -> Environment -> IO ()
defaultMain port env = do
  mapConcurrently_ id
    [ scheduleWorker env
    , runSettings settings (app env)
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
