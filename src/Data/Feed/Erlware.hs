module Data.Feed.Erlware (erlware) where

import Data.Feed.Parser

import Data.Function
       (fix)

import qualified Text.XML as XML

import Control.Lens
import qualified Data.Text.Strict.Lens as T
import Text.XML.Lens

import Network.URI.Lens.Extra
       (uriPathLens, _URI)

import Data.Time
       (defaultTimeLocale, parseTimeM)

title :: TextContentParser Element
title = contramap (foldOf (cosmos . named "title" . text)) textString

erlware :: FeedParser Element
erlware = fix $ \f -> FeedParser
  { entryParser = erlwareEntry
  , entryLocator = EntryLocator (toListOf (cosmos . named "a" . attributeIs "class" "post-card-content-link" . attr "href" . T.unpacked . to (fixUrl (origin f))))
  , titleParser = title
  , origin = "https://blog.erlware.org/"
  , slug = "erlware"
  }
  where
    fixUrl :: String -> String -> String
    fixUrl o u = o & set (_URI . uriPathLens) u

erlwareEntry :: EntryParser Element
erlwareEntry = fix $ \e -> EntryParser
  { entryTitleParser = title
  , entryUpdateParser = entryPublishedParser e
  , entryPublishedParser = contramap (parseTimeM True defaultTimeLocale "%Y-%m-%d" . dateTxt) day
  , entryContentParser = contramap (firstOf (cosmos . named "section" . attributeIs "class" "post-full-content" . to XML.toXMLElement)) xhtmlContent
  }
  where
    dateTxt = foldOf (cosmos . named "time" . attributeIs "class" "post-full-meta-date" . attr "datetime" . T.unpacked)
