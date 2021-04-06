module Network.HTTP.HasManager (HasManager(..), Manager) where

import Control.Lens

import Network.HTTP.Conduit
       (Manager)

class HasManager a where
  manager :: Lens' a Manager

instance HasManager Manager where
  manager = id
