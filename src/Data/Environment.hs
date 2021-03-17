module Data.Environment (Environment(..)) where

import Network.HTTP.HasManager (HasManager(..), Manager)
import System.Directory.HasCache (HasCache(..), Cache)

import GHC.Generics (Generic)
import Data.Generics.Product (typed)

data Environment
  = Environment { environmentManager :: Manager
                , environmentCache :: Cache
                }

deriving stock instance Generic Environment

instance HasManager Environment where
  manager = typed @Manager
instance HasCache Environment where
  cache = typed @Cache
