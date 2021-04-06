module Control.Monad.Logger
  ( Logger(..)
  , HasLogger(..)
  , LoggingM(..)
  , LogEnv(..)
  -- * Re-export katip
  , Katip
  , KatipContext
  )
  where

import Katip

import GHC.Generics
       (Generic)

import Control.Monad.Reader
       (MonadReader, local)
import Control.Monad.Trans
       (MonadIO)

import Control.Lens
import Data.Generics.Product
       (typed)

data Logger
  = Logger { logEnvNamespace :: Namespace
           , logEnvLogContext :: LogContexts
           , logEnvLogEnv :: LogEnv
           }
  deriving stock (Generic)

class HasLogger a where
  logger :: Lens' a Logger

newtype LoggingM m r a = LoggingM ( m a )
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader r) via m

instance (MonadIO m, MonadReader r m, HasLogger r) => Katip (LoggingM m r) where
  getLogEnv = view (logger . typed @LogEnv)
  localLogEnv f = local (over (logger . typed @LogEnv) f)

instance (MonadIO m, MonadReader r m, HasLogger r) => KatipContext (LoggingM m r) where
  getKatipContext = view (logger . typed @LogContexts)
  localKatipContext f = local (over (logger . typed @LogContexts) f)

  getKatipNamespace = view (logger . typed @Namespace)
  localKatipNamespace f = local (over (logger . typed @Namespace) f)
