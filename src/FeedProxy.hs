{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module FeedProxy where

import qualified Text.XML as XML
import Data.Functor.Coyoneda (Coyoneda(..), liftCoyoneda)
import Control.Monad.Free.Church
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Conduit
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Text.HTML.DOM as HTML
import Control.Lens
import Text.XML.Lens
import Debug.Trace (traceM)
import Network.URI.Lens.Extra
       (uriPathLens, _URI)
import qualified Data.Text.Lens as T
import Data.Text (Text)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import qualified Database.SQLite.Simple as SQL
import Control.Exception (Exception)
import Control.Monad.Catch (throwM)
import Database (runMigrations)

-- The idea behind this module is to make the existing feed-proxy more modular
-- by allowing users to declare their own feeds and parsers, in a style similar
-- to xmonad

data Entry = Entry
  { entryTitle :: Text
  , entryLink :: String
  , entryContent :: Maybe Element
  , entryUpdated :: Maybe UTCTime
  } deriving (Show)

type XML = XML.Document

data Feed a = Feed
  { feedSource :: URL
  , feedTitle :: Text
  , feedEntries :: a
  } deriving (Show)

type Configuration = Feed (XML -> EffectM [Entry])

type URL = String

-- I want to limit the extent of the effects
data Effect a where
  FetchPage :: URL -> Effect LBS.ByteString

-- | EffectM is the monad for limiting the effects within the Feed declaration
type EffectM = F (Coyoneda Effect)

fetchPage :: String -> EffectM LBS.ByteString
fetchPage url = liftF (liftCoyoneda (FetchPage url))

fetchPage' :: SQL.Connection -> Manager -> URL -> IO LBS.ByteString
fetchPage' conn manager url = do
  -- Try to fetch from cache, if it exists otherwise fetch from remote
  content <- fetchFromCache conn url
  case content of
    Just content' -> return content'
    Nothing -> do
      content' <- fetchRemote manager url
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
fetchRemote manager url = do
  request <- parseRequest url
  response <- httpLbs request manager
  return $ responseBody response

runEffectM :: SQL.Connection -> Manager -> EffectM a -> IO a
runEffectM conn manager = iterM $ \case
  Coyoneda k (FetchPage url) -> fetchPage' conn manager url >>= k


-- TODO: Move this function out of this module
autotie :: URL -> Text -> Configuration
autotie source title = Feed source title $ \doc -> do
  -- Find the urls for the blog entries
  let urls = map toAbsolutePath $ toListOf blogUrlLens doc
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
      }

poloinen :: Configuration
poloinen = autotie "https://www.autotie.fi/tien-sivusta/poloinen" "Poloinen"

parseHtml :: LBS.ByteString -> XML
parseHtml = HTML.parseLBS

evalConfiguration :: SQL.Connection -> Manager -> Configuration -> IO (Feed [Entry])
evalConfiguration conn manager conf =
  runEffectM conn manager $ do
    base <- parseHtml <$> fetchPage (feedSource conf)
    entries <- feedEntries conf base
    pure Feed
      { feedSource = feedSource conf
      , feedTitle = feedTitle conf
      , feedEntries = entries
      }

test :: IO ()
test = SQL.withConnection "feeds.db" $ \conn -> do
  runMigrations conn
  manager <- newTlsManager
  traceM "foo"
  evalConfiguration conn manager poloinen >>= print
