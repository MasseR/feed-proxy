module Data.Feed.Autotie
  ( autotie )
  where

import Data.Feed.Parser

import Control.Lens
import qualified Data.Text.Strict.Lens as T
import Text.XML.Lens

import qualified Text.XML as XML

import Data.Function
       (fix)

import Network.URI.Lens.Extra
       (uriPathLens, _URI)

import Data.Time
       (defaultTimeLocale, parseTimeM)
import Data.Text (Text)

title :: TextContentParser Element
title = contramap (foldOf (cosmos . named "title" . text)) textString

autotie :: Text -> String -> FeedParser Element
autotie slug origin = FeedParser
  { entryParser = autotieEntry
  , entryLocator = EntryLocator (map (fixUrl origin) . toListOf (cosmos . named "a" . attributeIs "class" "blogi-kortti" . attr "href" . T.unpacked))
  , titleParser = title
  , origin = origin
  , slug = slug
  -- , origin = "https://www.autotie.fi/tien-sivusta/sahkoautoileva-motoristi"
  -- , slug = "autoileva-motoristi"
  }
  where
    fixUrl o u = o & set (_URI . uriPathLens) u

autotieEntry :: EntryParser Element
autotieEntry = fix $ \e -> EntryParser
  { entryTitleParser = title
  , entryUpdateParser = entryPublishedParser e
  , entryPublishedParser = contramap (parseTimeM True defaultTimeLocale "%d.%m.%Y" . dateTxt) day
  , entryContentParser = contramap (fmap XML.toXMLElement . firstOf (cosmos . named "div" . attributeIs "class" "blogikirjoitus_kirjoitus")) xhtmlContent
  }
  where
    dateTxt = foldOf (cosmos . named "div" . attributeIs "class" "blogikirjoitus" ... named "p" ... named "span" . text . T.unpacked)

