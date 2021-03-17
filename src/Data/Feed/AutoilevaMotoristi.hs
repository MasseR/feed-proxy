module Data.Feed.AutoilevaMotoristi
  ( autoilevaMotoristi )
  where

import Data.Feed.Parser

import Text.XML.Lens
import Control.Lens
import qualified Data.Text.Strict.Lens as T

import qualified Text.XML as XML

import Data.Function (fix)

import Network.URI.Lens.Extra (_URI, uriPathLens)

import Data.Time (parseTimeM, defaultTimeLocale)

title :: TextContentParser Element
title = contramap (foldOf (cosmos . named "title" . text)) textString

autoilevaMotoristi :: FeedParser Element
autoilevaMotoristi = fix $ \f -> FeedParser
  { entryParser = autoilevaMotoristiEntry
  , entryLocator = EntryLocator (map (fixUrl (origin f)) . toListOf (cosmos . named "a" . attributeIs "class" "blogi-kortti" . attr "href" . T.unpacked))
  , titleParser = title
  , origin = "https://www.autotie.fi/tien-sivusta/sahkoautoileva-motoristi"
  , slug = "autoileva-motoristi"
  }
  where
    fixUrl o u = o & set (_URI . uriPathLens) u

autoilevaMotoristiEntry :: EntryParser Element
autoilevaMotoristiEntry = fix $ \e -> EntryParser
  { entryTitleParser = title
  , entryUpdateParser = entryPublishedParser e
  , entryPublishedParser = contramap (parseTimeM True defaultTimeLocale "%d.%m.%Y" . dateTxt) day
  , entryContentParser = contramap (fmap XML.toXMLElement . firstOf (cosmos . named "div" . attributeIs "class" "blogikirjoitus_kirjoitus")) xhtmlContent
  }
  where
    dateTxt = foldOf (cosmos . named "div" . attributeIs "class" "blogikirjoitus" ... named "p" ... named "span" . text . T.unpacked)
