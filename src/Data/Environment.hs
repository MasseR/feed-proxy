module Data.Environment (Environment(..)) where

import Network.HTTP.HasManager (HasManager(..), Manager)

data Environment
  = Environment { environmentManager :: Manager }
