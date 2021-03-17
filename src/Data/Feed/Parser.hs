module Data.Feed.Parser
  ( FeedParser(..)
  , EntryParser(..)
  , EntryLocator(..)
  , TextParser(..)
  , TextContentParser(..)
  , TimeParser(..)
  , ContentParser(..)
  , day
  , textString
  , xhtmlContent
  , contramap
  )
  where

import Text.Atom.Feed (TextContent(..), EntryContent(..))

import Data.Text (Text)

import Data.Time (UTCTime(..), Day)

import Data.Functor.Contravariant

import Data.XML.Types (Element)

-- | The feed parser
--
-- Represents the parsing of a single feed
data FeedParser a
  = FeedParser { entryParser :: EntryParser a
               , entryLocator :: EntryLocator a
               , titleParser :: TextContentParser a
               , origin :: String
               , slug :: Text
               }

-- | The entry parser
--
-- Represents parsing of a single entry
data EntryParser a
  = EntryParser { entryTitleParser :: TextContentParser a
                , entryUpdateParser :: TimeParser a
                , entryPublishedParser :: TimeParser a
                , entryContentParser :: ContentParser a
                }

newtype EntryLocator a = EntryLocator { getEntryLocator :: a -> [String] }
newtype TextParser a = TextParser { getTextParser :: a -> Text }
newtype TextContentParser a = TextContentParser (a -> TextContent)
newtype TimeParser a = TimeParser { getTimeParser :: a -> Maybe UTCTime }
newtype ContentParser a = ContentParser { getContentParser :: a -> Maybe EntryContent }

day :: TimeParser (Maybe Day)
day = TimeParser (fmap (\d -> UTCTime d 0))

textString :: TextContentParser Text
textString = TextContentParser TextString

xhtmlContent :: ContentParser (Maybe Element)
xhtmlContent = ContentParser (fmap XHTMLContent)

deriving via (Op [String]) instance Contravariant EntryLocator
deriving via (Op Text) instance Contravariant TextParser
deriving via (Op TextContent) instance Contravariant TextContentParser
deriving via (Op (Maybe UTCTime)) instance Contravariant TimeParser
deriving via (Op (Maybe EntryContent)) instance Contravariant ContentParser
