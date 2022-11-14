{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
-- An in-memory cache, storing bytestrings in memory
module Cache
  ( Cache(..)
  , HasCache(..)
  , newCache
  , readCache
  , writeCache
  ) where

import Control.Lens
import Data.Map (Map)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Control.Concurrent.STM (TVar)
import Data.Time (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)
import qualified Data.Map.Strict as M
import Control.Monad.Reader (MonadReader)
import UnliftIO (MonadIO, liftIO, atomically, readTVar, newTVarIO, modifyTVar)

data CacheEntry = CacheEntry { _cacheContent :: ByteString, _cacheExpires :: UTCTime }
                deriving Show
newtype Cache = Cache { _cacheEntries :: TVar (Map Text CacheEntry) }

makeLenses ''CacheEntry
makeLenses ''Cache

-- | Build a new empty cache
newCache :: IO Cache
newCache = Cache <$> newTVarIO M.empty

class HasCache a where
  cache :: Lens' a Cache

instance HasCache Cache where
  cache = id

-- | Try to get a value from cache
--
-- Will return Nothing if the element has expired
-- Will return Nothing if the element is not found
readCache :: (MonadReader r m, MonadIO m, HasCache r) => Text -> m (Maybe ByteString)
readCache key = do
  now <- liftIO getCurrentTime
  c <- view (cache . cacheEntries)
  atomically $ do
    preview (ix key . filtered (\x -> now < view cacheExpires x ) . cacheContent) <$> readTVar c

-- | Updates the given value to the cache, replacing the existing value
writeCache :: (MonadReader r m, MonadIO m, HasCache r) => Text -> NominalDiffTime -> ByteString -> m ()
writeCache key expires value = do
  now <- liftIO getCurrentTime
  c <- view (cache . cacheEntries)
  atomically $ do
    let entry = CacheEntry { _cacheContent = value, _cacheExpires = expires `addUTCTime` now }
    modifyTVar c (set (at key) (Just entry))
