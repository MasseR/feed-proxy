{-# LANGUAGE TemplateHaskell #-}
module System.Directory.HasCache
  ( Cache(..)
  , HasCache(..)
  , cachePath
  ) where

import Control.Lens

newtype Cache = Cache { _cachePath :: FilePath }

makeLenses ''Cache

class HasCache a where
  cache :: Lens' a Cache

instance HasCache Cache where
  cache = id
