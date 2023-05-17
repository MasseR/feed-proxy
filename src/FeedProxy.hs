{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module FeedProxy where

import Control.Exception (Exception)
import Control.Lens
import Control.Monad.Catch (throwM)
import Control.Monad.Free.Church
import qualified Data.ByteString.Lazy as LBS
import Data.Functor.Coyoneda (Coyoneda(..), liftCoyoneda)
import Data.Text (Text)
import qualified Data.Text.Lens as T
import Data.Time (defaultTimeLocale, parseTimeM)
import qualified Database.SQLite.Simple as SQL
import Network.HTTP.Conduit
import Network.URI.Lens.Extra (_URI, uriPathLens)
import qualified Text.HTML.DOM as HTML
import Text.XML.Lens

import FeedProxy.Feed
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Database (HasConnection, connection)
import Network.HTTP.HasManager (HasManager, manager)

-- The idea behind this module is to make the existing feed-proxy more modular
-- by allowing users to declare their own feeds and parsers, in a style similar
-- to xmonad


-- | The configuration defines how to parse a web page into entries
type Configuration = Feed' (LBS.ByteString -> EffectM [Entry])

-- I want to limit the extent of the effects
data Effect a where
  FetchPage :: URL -> Effect LBS.ByteString

-- | EffectM is the monad for limiting the effects within the Feed declaration
type EffectM = F (Coyoneda Effect)

fetchPage :: String -> EffectM LBS.ByteString
fetchPage url = liftF (liftCoyoneda (FetchPage url))

fetchPage' :: SQL.Connection -> Manager -> URL -> IO LBS.ByteString
fetchPage' conn mgr url = do
  -- Try to fetch from cache, if it exists otherwise fetch from remote
  content <- fetchFromCache conn url
  case content of
    Just content' -> return content'
    Nothing -> do
      content' <- fetchRemote mgr url
      _ <- SQL.execute conn "insert into cache (link, content, time) values (?, ?, datetime('now'))" (url, content')
      return content'

fetchFromCache :: SQL.Connection -> URL -> IO (Maybe LBS.ByteString)
fetchFromCache conn url = do
  res <- SQL.query conn "select content from cache where link = ? and time < datetime('now', '+1 hour')" (SQL.Only url)
  case res of
    [SQL.Only content] -> return $ Just content
    [] -> return Nothing
    _ -> throwM TooManyResults

data UnexpectedResults = TooManyResults
  deriving (Show)

instance Exception UnexpectedResults

fetchRemote :: Manager -> URL -> IO LBS.ByteString
fetchRemote mgr url = do
  request <- parseRequest url
  let request' = request{requestHeaders = [("User-Agent", "feed-proxy")]}
  response <- httpLbs request' mgr
  return $ responseBody response

runEffectM :: SQL.Connection -> Manager -> EffectM a -> IO a
runEffectM conn mgr = iterM $ \case
  Coyoneda k (FetchPage url) -> fetchPage' conn mgr url >>= k

erlware :: Configuration
erlware = Feed source "Erlware" "erlware" $ \doc -> do
  -- Find the urls for the blog entries
  let urls = map toAbsolutePath $ toListOf blogUrlLens $ parseHtml doc
  -- Fetch all the urls and parse them into entries
  traverse (\u -> fmap (parseEntry u) . fetchPage $ u) urls
  where
    source = "https://blog.erlware.org"
    -- Parse the blog entries
    parseEntry :: String -> LBS.ByteString -> Entry
    parseEntry url = xmlToEntry url . parseHtml
    toAbsolutePath url = source & set (_URI . uriPathLens) url
    blogUrlLens = root . cosmos . named "a" . attributeIs "class" "post-card-content-link" . attr "href" . T.unpacked
    parseTimeLens = foldOf (root . cosmos . named "time" . attributeIs "class" "post-full-meta-date" . attr "datetime" . T.unpacked)
    xmlToEntry :: String -> XML -> Entry
    xmlToEntry url xml = Entry
      { entryTitle = foldOf (root . cosmos . named "title" . text) xml
      , entryLink = url
      , entryContent = firstOf (root . cosmos . named "section" . attributeIs "class" "post-full-content") xml
      , entryUpdated = parseTimeM True defaultTimeLocale "%Y-%m-%d" . parseTimeLens $ xml
      , entryLinks = []
      }

-- TODO: Move this function out of this module
autotie :: URL -> Text -> Text -> Configuration
autotie source title slug = Feed source title slug $ \doc -> do
  -- Find the urls for the blog entries
  let urls = map toAbsolutePath $ toListOf blogUrlLens $ parseHtml doc
  -- Fetch all the urls and parse them into entries
  traverse (\u -> fmap (parseEntry u) . fetchPage $ u) urls
  where
    -- Parse the blog entries
    parseEntry :: String -> LBS.ByteString -> Entry
    parseEntry url = xmlToEntry url . parseHtml
    blogUrlLens = root . cosmos . named "a" . attributeIs "class" "blogi-kortti" . attr "href" . T.unpacked
    toAbsolutePath url = source & set (_URI . uriPathLens) url
    parseTimeLens = foldOf (root . cosmos . named "div" . attributeIs "class" "blogikirjoitus" ... named "p" ... named "span" . text . T.unpacked)
    xmlToEntry :: String -> XML -> Entry
    xmlToEntry url xml = Entry
      { entryTitle = foldOf (root . cosmos . named "title" . text) xml
      , entryLink = url
      , entryContent = firstOf (root . cosmos . named "div" . attributeIs "class" "blogikirjoitus_kirjoitus") xml
      , entryUpdated = parseTimeM True defaultTimeLocale "%d.%m.%Y" . parseTimeLens $ xml
      , entryLinks = []
      }

poloinen :: Configuration
poloinen = autotie "https://www.autotie.fi/tien-sivusta/poloinen" "Poloinen" "poloinen"

autoilevaMotoristi :: Configuration
autoilevaMotoristi = autotie "https://www.autotie.fi/tien-sivusta/sahkoautoileva-motoristi" "Sähköautoileva motoristi" "autoileva-motoristi"

parseHtml :: LBS.ByteString -> XML
parseHtml = HTML.parseLBS

evalConfiguration :: (MonadIO m, MonadReader r m, HasConnection r, HasManager r) => Configuration -> m Feed
evalConfiguration conf = do
  conn <- view connection
  mgr <- view manager
  liftIO $ runEffectM conn mgr $ do
    base <- fetchPage (feedSource conf)
    entries <- feedEntries conf base
    pure Feed
      { feedSource = feedSource conf
      , feedTitle = feedTitle conf
      , feedEntries = entries
      , feedSlug = feedSlug conf
      }
