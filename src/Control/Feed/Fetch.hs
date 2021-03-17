{-# LANGUAGE TemplateHaskell #-}
module Control.Feed.Fetch (getFeed) where

import Data.Feed.Parser
import Data.Feed.Render

import Data.ByteString.Lazy (ByteString)

import Control.Lens
import qualified Data.Text.Strict.Lens as T

import Data.Time (UTCTime(..), defaultTimeLocale, getCurrentTime, formatTime, diffUTCTime)

import Data.Coerce (coerce)

import Control.Applicative ((<|>))

import Text.Atom.Feed (Entry(..), Feed(..))
import Text.Feed.Types.Lens
import Text.Feed.Import (parseFeedFromFile)

import Network.HTTP.Conduit
import Conduit (runResourceT)

import Control.Monad.Trans.Maybe (MaybeT(..))

import Control.Exception (IOException, handle)

import System.Directory (getModificationTime)

import qualified Data.ByteString.Lazy as LBS

import Data.Foldable (traverse_)

cacheName :: FeedParser a -> FilePath
cacheName f = (slug f ^. T.unpacked) <> ".cache"

getFromCache :: FeedParser a -> IO (Maybe Feed)
getFromCache f = handle @IOException (const $ pure Nothing) $ do
  modified <- getModificationTime (cacheName f)
  now <- getCurrentTime
  if diffUTCTime now modified > 300
     then pure Nothing
     else preview (_Just . _AtomFeed) <$> parseFeedFromFile (cacheName f)

writeCache :: FeedParser a -> Feed -> IO Feed
writeCache f feed =
  feed <$ traverse_ (LBS.writeFile (cacheName f)) (render feed)

downloadFeed :: Manager -> FeedParser (Response ByteString) -> IO Feed
downloadFeed manager f = do
  feedresponse <- runResourceT $ do
    request <- parseRequest (origin f)
    httpLbs request manager
  let entryUrls = getEntryLocator (entryLocator f) feedresponse
  now <- getCurrentTime
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
           <*> traverse (getEntry now (entryParser f)) entryUrls
           <*> pure []
           <*> pure []
  writeCache f feed
  where
    fmtTime = view T.packed . formatTime defaultTimeLocale "%d.%m.%Y"
    getEntry :: UTCTime -> EntryParser (Response ByteString) -> String -> IO Entry
    getEntry now e url = do
      entryresponse <- runResourceT $ do
        request <- parseRequest url
        httpLbs request manager
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


getFeed :: Manager -> FeedParser (Response ByteString) -> IO (Maybe Feed)
getFeed manager f = runMaybeT (MaybeT (getFromCache f) <|> MaybeT (Just <$> downloadFeed manager f))
