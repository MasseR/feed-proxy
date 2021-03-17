{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module MyLib (defaultMain) where

import Data.Environment
import Control.Monad.FeedProxy

import Control.Exception (try)
import Control.Monad.Trans.Except
import Control.Monad.Catch (throwM)

import Data.Feed.Render

import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Network.HTTP.Media.MediaType ((//))
import Network.Wai.Handler.Warp (run)

import Data.Text (Text)

import Text.Atom.Feed (Feed)

data Atom

instance Accept Atom where
  contentType _ = "application" // "atom+xml"

instance MimeRender Atom Feed where
  mimeRender _ = either (const "") id . render

data Routes route
  = Routes { _getFeed :: route :- Capture "slug" Text :> Get '[Atom] Feed }

deriving stock instance Generic (Routes route)

server :: Routes (AsServerT FeedProxyM)
server =
  Routes { _getFeed = \_ -> throwM err404{errBody="foo"} }

app :: Environment -> Application
app env = genericServeT nt server
  where
    nt :: FeedProxyM a -> Handler a
    nt = Handler . ExceptT . try @ServerError . runFeedProxy env

defaultMain :: Int -> Environment -> IO ()
defaultMain port env = do
  run port (app env)
