{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module MyLib (defaultMain) where

import Data.Feed.AutoilevaMotoristi
import Data.Feed.Erlware
import Data.Feed.AudibleNewReleases

import Data.Environment
import Control.Monad.FeedProxy
import Control.Monad ((<=<))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Applicative (empty)

import Control.Exception (try)
import Control.Monad.Trans.Except
import Control.Monad.Catch (throwM)

import Network.HTTP.Conduit (responseBody, Response)
import Data.ByteString.Lazy (ByteString)
import qualified Text.HTML.DOM as DOM

import Data.Feed.Render

import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Network.HTTP.Media.MediaType ((//))
import Network.Wai.Handler.Warp (run)

import Data.Text (Text)

import Data.Trace

import Text.Atom.Feed (Feed)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Feed.Parser (FeedParser(slug, origin))
import Text.XML (Element)

import Control.Lens
import Text.XML.Lens (root)

import Control.Feed.Fetch (getFeed, FetchTrace(..))

import qualified Katip as K

data Atom

instance Accept Atom where
  contentType _ = "application" // "atom+xml"

instance MimeRender Atom Feed where
  mimeRender _ = either (const "") id . render

data Routes route
  = Routes { _getFeed :: route :- "feed" :> Capture "slug" Text :> Get '[Atom] Feed
           , _getFeeds :: route :- "feed" :> Get '[JSON] [Text]
           }

deriving stock instance Generic (Routes route)

feeds :: Map Text (FeedParser Element)
feeds = foldMap (\p -> M.singleton (slug p) p)
  [ autoilevaMotoristi
  , autoilevaMotoristi{slug="poloinen", origin="https://www.autotie.fi/tien-sivusta/poloinen"}
  , erlware
  , audibleNewReleases
  ]

server :: Routes (AsServerT FeedProxyM)
server =
  Routes { _getFeed = maybe (throwM err404) pure <=< runFeed
         , _getFeeds = pure $ M.keys feeds
  }

runFeed :: Text -> FeedProxyM (Maybe Feed)
runFeed name = runMaybeT $ do
  parser <- toMaybeT (M.lookup name feeds)
  MaybeT (getFeed (contramap formatTrace logTrace) (contramap toElement parser))
  where
    toMaybeT = maybe empty pure
    toElement :: Response ByteString -> Element
    toElement lbs = DOM.parseLBS (responseBody lbs) ^. root

app :: Environment -> Application
app env = genericServeT nt server
  where
    nt :: FeedProxyM a -> Handler a
    nt = Handler . ExceptT . try @ServerError . runFeedProxy env

formatTrace :: FetchTrace -> K.LogStr
formatTrace = \case
  Fetch url -> "Fetching '" <> K.ls url <> "'"
  Hit url -> "Cache hit for '" <> K.ls url <> "'"
  Miss url -> "Cache miss for '" <> K.ls url <> "'"

logTrace :: (K.KatipContext m) => Trace m K.LogStr
logTrace = Trace (K.logFM K.InfoS)

defaultMain :: Int -> Environment -> IO ()
defaultMain port env = do
  run port (app env)
