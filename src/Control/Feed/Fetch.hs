{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Feed.Fetch (getFeed, FetchTrace(..)) where

import Data.Feed.Parser
import Data.Feed.Render

import Network.HTTP.HasManager
import System.Directory.HasCache
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO, liftIO, lift)

import Data.ByteString.Lazy (ByteString)

import Control.Lens
import qualified Data.Text.Strict.Lens as T

import Data.Time (UTCTime(..), getCurrentTime, diffUTCTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format)

import Data.Coerce (coerce)

import Control.Applicative ((<|>))

import UnliftIO.Async (pooledMapConcurrentlyN)
import UnliftIO (MonadUnliftIO)

import Text.Atom.Feed (Entry(..), Feed(..))
import Text.Feed.Types.Lens
import Text.Feed.Import (parseFeedFromFile)

import Network.HTTP.Conduit
import Conduit (runResourceT)

import Control.Monad.Trans.Maybe (MaybeT(..))

import Control.Exception (IOException)
import Control.Monad.Catch (handle)

import System.Directory (getModificationTime)

import qualified Data.ByteString.Lazy as LBS

import Data.Foldable (traverse_)

import System.FilePath ((</>))

import Data.Trace

type MonadFeed r m = (MonadReader r m, MonadIO m, HasManager r, HasCache r, MonadUnliftIO m)

data FetchTrace
  = Fetch String
  | Hit String
  | Miss String

cacheName :: FeedParser a -> FilePath
cacheName f = (slug f ^. T.unpacked) <> ".cache"

cachefile :: MonadFeed r m => FeedParser a -> m FilePath
cachefile f = do
  p <- view (cache . cachePath)
  pure (p </> cacheName f)

getFromCache :: MonadFeed r m => FeedParser a -> m (Maybe Feed)
getFromCache f = do
  filename <- cachefile f
  liftIO $ handle @_ @IOException (const $ pure Nothing) $ do
    modified <- getModificationTime filename
    now <- getCurrentTime
    if diffUTCTime now modified > 300
       then pure Nothing
       else preview (_Just . _AtomFeed) <$> parseFeedFromFile filename

writeCache :: MonadFeed r m => FeedParser a -> Feed -> m Feed
writeCache f feed = do
  filename <- cachefile f
  feed <$ traverse_ (liftIO . LBS.writeFile filename) (render feed)

downloadFeed :: MonadFeed r m => Manager -> FeedParser (Response ByteString) -> m Feed
downloadFeed mgr f = do
  feedresponse <- liftIO $ runResourceT $ do
    request <- parseRequest (origin f)
    httpLbs request mgr
  let entryUrls = getEntryLocator (entryLocator f) feedresponse
  now <- liftIO $ getCurrentTime
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

getFeed :: MonadFeed r m => Trace m FetchTrace -> FeedParser (Response ByteString) -> m (Maybe Feed)
getFeed tracer f = do
  mgr <- view manager
  trace tracer (Fetch url)
  runMaybeT (hit (MaybeT (getFromCache f)) <|> MaybeT (Just <$> miss (downloadFeed mgr f)))
  where
    url = origin f
    hit x = x <* lift (trace tracer (Hit url))
    miss x = x <* trace tracer (Miss url)
