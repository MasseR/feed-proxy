module Data.Environment (Environment(..)) where

import Network.HTTP.HasManager (HasManager(..), Manager)
import System.Directory.HasCache (HasCache(..), Cache)

import Control.Monad.Logger (Logger(..), HasLogger(..))

import GHC.Generics (Generic)
import Data.Generics.Product (typed)

data Environment
  = Environment { environmentManager :: Manager
                , environmentCache :: Cache
                , environmentLogger :: Logger
                }

deriving stock instance Generic Environment

instance HasManager Environment where
  manager = typed @Manager
instance HasCache Environment where
  cache = typed @Cache
instance HasLogger Environment where
  logger = typed @Logger
