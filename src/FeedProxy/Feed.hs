module FeedProxy.Feed where

import Control.Lens (view)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Max(..))
import Data.Text (Text)
import qualified Data.Text.Lens as T
import Data.Time (UTCTime(..), fromGregorian)
import Data.Time.Format.ISO8601
import qualified Text.Atom.Feed as Atom
import qualified Text.XML as XML
import Text.XML (Element)

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

type URL = String

-- | The type corresponds to either a fully parsed feed or a declarative
-- configuration on how to parse something into a list of entries
data Feed' a = Feed
  { feedSource :: URL
  , feedTitle :: Text
  , feedSlug :: Text
  , feedEntries :: a -- ^ A list of entries
  } deriving (Show)

type Feed = Feed' [Entry]

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
