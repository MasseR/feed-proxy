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
import Data.Time (UTCTime(..), parseTimeM, defaultTimeLocale, fromGregorian)
import qualified Database.SQLite.Simple as SQL
import Control.Exception (Exception)
import Control.Monad.Catch (throwM)
import Database (runMigrations)

import qualified Text.Atom.Feed as Atom
import Data.Time.Format.ISO8601
import Data.Semigroup (Max(..))
import Data.Maybe (fromMaybe)
import qualified Data.Feed.Render as Feed

-- The idea behind this module is to make the existing feed-proxy more modular
-- by allowing users to declare their own feeds and parsers, in a style similar
-- to xmonad


-- | A link is a link to an external resource. This could be for example a link
-- to a video or an audio file in case of a podcast.
--
-- A link consists of href, title, optional rel and optional content type
data Link = Link
  { linkHref :: Text
  , linkTitle :: Text
  , linkRel :: Maybe Text
  , linkContentType :: Maybe Text
  } deriving (Show)

-- | Entry represents a single article entry
data Entry = Entry
  { entryTitle :: Text
  , entryLink :: String
  , entryContent :: Maybe Element
  , entryUpdated :: Maybe UTCTime
  , entryLinks :: [Link]
  } deriving (Show)

type XML = XML.Document

-- | The type corresponds to either a fully parsed feed or a declarative
-- configuration on how to parse something into a list of entries
data Feed' a = Feed
  { feedSource :: URL
  , feedTitle :: Text
  , feedEntries :: a -- ^ A list of entries
  } deriving (Show)

-- | The configuration defines how to parse a web page into entries
type Configuration = Feed' (XML -> EffectM [Entry])

type Feed = Feed' [Entry]

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
      , entryLinks = []
      }

poloinen :: Configuration
poloinen = autotie "https://www.autotie.fi/tien-sivusta/poloinen" "Poloinen"

autoilevaMotoristi :: Configuration
autoilevaMotoristi = autotie "autoileva-motoristi" "https://www.autotie.fi/tien-sivusta/sahkoautoileva-motoristi"

parseHtml :: LBS.ByteString -> XML
parseHtml = HTML.parseLBS

evalConfiguration :: SQL.Connection -> Manager -> Configuration -> IO Feed
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
  evalConfiguration conn manager poloinen >>= print . Feed.render . feedToAtom
  evalConfiguration conn manager autoilevaMotoristi >>= print


feedToAtom :: Feed -> Atom.Feed
feedToAtom feed = Atom.Feed
  { Atom.feedTitle = Atom.TextString $ feedTitle feed
  , Atom.feedEntries = map entryToAtom $ feedEntries feed
  , Atom.feedLinks = []
  , Atom.feedUpdated = formatTime . latestUpdated $ feedEntries feed
  , Atom.feedId = view T.packed $ feedSource feed -- The id is the source link
  , Atom.feedGenerator = Nothing
  , Atom.feedCategories = []
  , Atom.feedContributors = []
  , Atom.feedRights = Nothing
  , Atom.feedSubtitle = Nothing
  , Atom.feedAuthors = []
  , Atom.feedOther = []
  , Atom.feedAttrs = []
  , Atom.feedIcon = Nothing
  , Atom.feedLogo = Nothing
  }
  where
    -- Find the latest updated time from the entries
    latestUpdated :: [Entry] -> UTCTime
    latestUpdated = maybe zeroUTCTime getMax . foldMap (fmap Max . entryUpdated)

zeroUTCTime :: UTCTime
zeroUTCTime = UTCTime (fromGregorian 1970 1 1) 0

-- Format time in ISO8601 format
formatTime :: UTCTime -> Text
formatTime = view T.packed . formatShow iso8601Format

-- Convert an Entry to Atom.Entry
entryToAtom :: Entry -> Atom.Entry
entryToAtom entry = Atom.Entry
  { Atom.entryId = view T.packed $ entryLink entry -- The id is the entry link
  , Atom.entryTitle = Atom.TextString $ entryTitle entry
  , Atom.entryUpdated = formatTime $ fromMaybe zeroUTCTime $ entryUpdated entry
  , Atom.entryAuthors = []
  , Atom.entryCategories = []
  , Atom.entryContent = Atom.XHTMLContent . XML.toXMLElement <$> entryContent entry
  , Atom.entryContributor = []
  , Atom.entryLinks = map linkToAtom $ entryLinks entry
  , Atom.entryPublished = formatTime <$> entryUpdated entry
  , Atom.entryRights = Nothing
  , Atom.entrySource = Nothing
  , Atom.entrySummary = Nothing
  , Atom.entryInReplyTo = Nothing
  , Atom.entryInReplyTotal = Nothing
  , Atom.entryAttrs = []
  , Atom.entryOther = []
  }
  where
    linkToAtom :: Link -> Atom.Link
    linkToAtom link = Atom.Link
      { Atom.linkHref = linkHref link
      , Atom.linkRel = Right  <$> linkRel link
      , Atom.linkType = linkContentType link
      , Atom.linkHrefLang = Nothing
      , Atom.linkTitle = Just $ linkTitle link
      , Atom.linkLength = Nothing
      , Atom.linkAttrs = []
      , Atom.linkOther = []
      }
