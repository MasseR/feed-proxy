{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module MyLib (defaultMain) where

import Data.Feed.AudibleNewReleases
import Data.Feed.AutoilevaMotoristi
import Data.Feed.Erlware

import Control.Applicative
       (empty)
import Control.Monad
       ((<=<))
import Control.Monad.FeedProxy
import Control.Monad.Trans.Maybe
       (MaybeT(..))
import Data.Environment

import Control.Exception
       (try)
import Control.Monad.Catch
       (throwM)
import Control.Monad.Trans.Except

import Data.ByteString.Lazy
       (ByteString)
import Network.HTTP.Conduit
       (Response, responseBody)
import qualified Text.HTML.DOM as DOM

import Data.Feed.Render

import Network.HTTP.Media.MediaType
       ((//))
import Network.Wai.Handler.Warp
       (run)
import Servant
import Servant.API.Generic
import Servant.Server.Generic

import Data.Text
       (Text)

import Text.Atom.Feed
       (Feed)

import Data.Feed.Parser
       (FeedParser(origin, slug))
import Data.Map
       (Map)
import qualified Data.Map as M
import Text.XML
       (Element)

import Control.Lens
import Text.XML.Lens
       (root)

import Control.Feed.Fetch
       (getFeed)

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
  MaybeT (getFeed (contramap toElement parser))
  where
    toMaybeT = maybe empty pure
    toElement :: Response ByteString -> Element
    toElement lbs = DOM.parseLBS (responseBody lbs) ^. root

app :: Environment -> Application
app env = genericServeT nt server
  where
    nt :: FeedProxyM a -> Handler a
    nt = Handler . ExceptT . try @ServerError . runFeedProxy env

defaultMain :: Int -> Environment -> IO ()
defaultMain port env = do
  run port (app env)
