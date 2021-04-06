{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.FeedProxy (FeedProxyM(..), runFeedProxy) where

import Control.Monad.Catch
import Control.Monad.Reader

import Data.Environment

import UnliftIO
       (MonadUnliftIO(..))

newtype FeedProxyM a = FeedProxyM ( ReaderT Environment IO a)

runFeedProxy :: Environment -> FeedProxyM a -> IO a
runFeedProxy env (FeedProxyM f) = runReaderT f env

deriving via (ReaderT Environment IO) instance Functor FeedProxyM
deriving via (ReaderT Environment IO) instance Applicative FeedProxyM
deriving via (ReaderT Environment IO) instance Monad FeedProxyM
deriving via (ReaderT Environment IO) instance MonadIO FeedProxyM
deriving via (ReaderT Environment IO) instance (MonadReader Environment) FeedProxyM
deriving via (ReaderT Environment IO) instance MonadThrow FeedProxyM
deriving via (ReaderT Environment IO) instance MonadCatch FeedProxyM
deriving via (ReaderT Environment IO) instance MonadUnliftIO FeedProxyM
