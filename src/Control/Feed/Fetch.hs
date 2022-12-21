{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
module Control.Feed.Fetch (cacheRefresher, getFeed, FetchTrace(..)) where

import Data.Feed.Parser
import Data.Feed.Render

import Control.Monad.Reader
       (MonadReader)
import Control.Monad.Trans
       (MonadIO, lift, liftIO)
import Network.HTTP.HasManager
import qualified Cache
import Cache (HasCache)

import Data.ByteString.Lazy
       (ByteString)

import Control.Lens
import qualified Data.Text.Strict.Lens as T

import Data.Time
       (UTCTime(..), getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Format.ISO8601
       (formatShow, iso8601Format)

import Data.Coerce
       (coerce)

import Control.Applicative
       ((<|>))

import UnliftIO
       (MonadUnliftIO, bracket)
import UnliftIO.Async
       (pooledMapConcurrentlyN)

import Text.Atom.Feed
       (Entry(..), Feed(..))
import Text.Feed.Import
       (parseFeedSource)
import Text.Feed.Types.Lens

import Conduit
       (runResourceT)
import Network.HTTP.Conduit

import Control.Monad.Trans.Maybe
       (MaybeT(..))



import qualified Data.ByteString.Lazy as LBS

import Data.Foldable
       (traverse_)


import Data.Trace
import Data.Time.Clock (NominalDiffTime)
import Control.Monad (forever, (<=<))
import Control.Concurrent (threadDelay)
import Control.Exception.Annotated.UnliftIO (checkpoint, Annotation (..), checkpointCallStack)
import qualified Data.ByteString as B
import System.CPUTime (getCPUTime)

type MonadFeed r m = (MonadReader r m, MonadIO m, HasManager r, HasCache r, MonadUnliftIO m)

data FetchTrace
  = FetchNew Double -- seconds
  | Hit
  | Miss
  | Scheduled

-- | Entries older than this are considered as expired
cacheExpireTime :: NominalDiffTime
cacheExpireTime = 15 * 60

getFromCache :: MonadFeed r m => FeedParser a -> m (Maybe Feed)
getFromCache f = checkpointCallStack $ do
  parse <$> Cache.readCache (slug f)
  where
    parse :: Maybe B.ByteString -> Maybe Feed
    parse = preview _AtomFeed <=< parseFeedSource <=< fmap LBS.fromStrict


writeCache :: MonadFeed r m => FeedParser a -> Feed -> m ()
writeCache f feed = checkpointCallStack $ do
  traverse_ go (render feed)
  where
    go lbs = do
      let !bs = LBS.toStrict lbs
      Cache.writeCache (slug f) cacheExpireTime bs

timed :: MonadUnliftIO m => (Double -> m ()) -> m a -> m a
timed f action = bracket start stop $ \_ -> action
  where
    start = liftIO getCPUTime
    stop begin = do
      end <- liftIO getCPUTime
      -- picoseconds to seconds
      let diff = fromIntegral (end - begin) / 1_000_000_000_000
      f diff

downloadFeed :: MonadFeed r m => Trace m FetchTrace -> Manager -> FeedParser (Response ByteString) -> m ()
downloadFeed tracer mgr f = checkpointCallStack $ timed (trace tracer . FetchNew) $ do
  feedresponse <- liftIO $ runResourceT $ do
    request <- parseRequest (origin f)
    httpLbs request mgr
  let entryUrls = getEntryLocator (entryLocator f) feedresponse
  now <- liftIO getCurrentTime
  feed <- Feed <$> pure (origin f ^. T.packed)
           <*> pure (coerce (titleParser f) feedresponse)
           <*> pure (fmtTime now)
           <*> pure []
           <*> pure []
           <*> pure []
           <*> pure Nothing
           <*> pure Nothing
           <*> pure []
           <*> pure Nothing
           <*> pure Nothing
           <*> pure Nothing
           <*> pooledMapConcurrentlyN 5 (liftIO . getEntry now (entryParser f)) entryUrls
           <*> pure []
           <*> pure []
  writeCache f feed
  where
    fmtTime = view T.packed . formatShow iso8601Format
    getEntry :: UTCTime -> EntryParser (Response ByteString) -> String -> IO Entry
    getEntry now e url = do
      entryresponse <- runResourceT $ do
        request <- parseRequest url
        httpLbs request mgr
      pure $ Entry
        (url ^. T.packed)
        (coerce (entryTitleParser e) entryresponse)
        (maybe "" fmtTime (getTimeParser (entryUpdateParser e) entryresponse <|> Just now))
        []
        []
        (coerce (entryContentParser e) entryresponse)
        []
        []
        (fmap fmtTime (getTimeParser (entryPublishedParser e) entryresponse <|> Just now))
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        []
        []


-- Refresh caches periodically
-- meant to be run in a separate thread
cacheRefresher :: MonadFeed r m => Trace m FetchTrace -> FeedParser (Response ByteString) -> m a
cacheRefresher tracer f = view manager >>= \mgr -> forever $ checkpoint (Annotation $ origin f) $ do
  trace tracer Scheduled
  _ <- downloadFeed tracer mgr f
  let waitFor = floor (1_000_000 * nominalDiffTimeToSeconds (cacheExpireTime - 60))
  liftIO $ threadDelay waitFor

getFeed :: MonadFeed r m => Trace m FetchTrace -> FeedParser (Response ByteString) -> m (Maybe Feed)
getFeed tracer f = checkpoint (Annotation $ origin f) $ do
  mgr <- view manager
  runMaybeT (hit (MaybeT (getFromCache f)) <|> MaybeT (miss (downloadFeed tracer mgr f) >> getFromCache f))
  where
    hit x = x <* lift (trace tracer Hit)
    miss x = x <* trace tracer Miss
