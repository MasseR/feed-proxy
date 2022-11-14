{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NumericUnderscores #-}
module Control.Feed.Fetch (cacheRefresher, getFeed, FetchTrace(..)) where

import Data.Feed.Parser
import Data.Feed.Render

import Control.Monad.Reader
       (MonadReader)
import Control.Monad.Trans
       (MonadIO, lift, liftIO)
import Network.HTTP.HasManager
import System.Directory.HasCache

import Data.ByteString.Lazy
       (ByteString)

import Control.Lens
import qualified Data.Text.Strict.Lens as T

import Data.Time
       (UTCTime(..), diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Format.ISO8601
       (formatShow, iso8601Format)

import Data.Coerce
       (coerce)

import Control.Applicative
       ((<|>))

import UnliftIO
       (MonadUnliftIO)
import UnliftIO.Async
       (pooledMapConcurrentlyN)

import Text.Atom.Feed
       (Entry(..), Feed(..))
import Text.Feed.Import
       (parseFeedFromFile)
import Text.Feed.Types.Lens

import Conduit
       (runResourceT)
import Network.HTTP.Conduit

import Control.Monad.Trans.Maybe
       (MaybeT(..))

import Control.Exception
       (IOException)
import Control.Monad.Catch
       (handle)

import System.Directory
       (getModificationTime)

import qualified Data.ByteString.Lazy as LBS

import Data.Foldable
       (traverse_)

import System.FilePath
       ((</>))

import Data.Trace
import Data.Time.Clock (NominalDiffTime)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Exception.Annotated.UnliftIO (checkpoint, Annotation (..), checkpointCallStack)

type MonadFeed r m = (MonadReader r m, MonadIO m, HasManager r, HasCache r, MonadUnliftIO m)

data FetchTrace
  = FetchNew
  | Hit
  | Miss
  | Scheduled

cacheName :: FeedParser a -> FilePath
cacheName f = (slug f ^. T.unpacked) <> ".cache"

cachefile :: MonadFeed r m => FeedParser a -> m FilePath
cachefile f = do
  p <- view (cache . cachePath)
  pure (p </> cacheName f)

-- | Entries older than this are considered as expired
cacheExpireTime :: NominalDiffTime
cacheExpireTime = 15 * 60

getFromCache :: MonadFeed r m => FeedParser a -> m (Maybe Feed)
getFromCache f = checkpointCallStack $ do
  filename <- cachefile f
  liftIO $ handle @_ @IOException (const $ pure Nothing) $ do
    modified <- getModificationTime filename
    now <- getCurrentTime
    if diffUTCTime now modified > cacheExpireTime
       then pure Nothing
       else preview (_Just . _AtomFeed) <$> parseFeedFromFile filename


writeCache :: MonadFeed r m => FeedParser a -> Feed -> m ()
writeCache f feed = checkpointCallStack $ do
  filename <- cachefile f
  traverse_ (liftIO . LBS.writeFile filename) (render feed)

downloadFeed :: MonadFeed r m => Manager -> FeedParser (Response ByteString) -> m ()
downloadFeed mgr f = checkpointCallStack $ do
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
  _ <- downloadFeed mgr f
  let waitFor = floor (1_000_000 * nominalDiffTimeToSeconds (cacheExpireTime - 60))
  liftIO $ threadDelay waitFor

getFeed :: MonadFeed r m => Trace m FetchTrace -> FeedParser (Response ByteString) -> m (Maybe Feed)
getFeed tracer f = checkpoint (Annotation $ origin f) $ do
  mgr <- view manager
  trace tracer FetchNew
  runMaybeT (hit (MaybeT (getFromCache f)) <|> MaybeT (miss (downloadFeed mgr f) >> getFromCache f))
  where
    hit x = x <* lift (trace tracer Hit)
    miss x = x <* trace tracer Miss
