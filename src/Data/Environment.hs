module Data.Environment (Environment(..)) where

import Network.HTTP.HasManager (HasManager(..), Manager)

import Control.Monad.Logger (HasLogger(..), Logger(..))

import Data.Generics.Product (typed)
import Database (HasConnection, connection)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)

data Environment
  = Environment { environmentManager :: Manager
                , environmentLogger :: Logger
                , environmentConnection :: Connection
                }

deriving stock instance Generic Environment

instance HasManager Environment where
  manager = typed @Manager
instance HasLogger Environment where
  logger = typed @Logger
instance HasConnection Environment where
  connection = typed @Connection
