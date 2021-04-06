module Data.Feed.AudibleNewReleases
  (audibleNewReleases)
  where

import Data.Feed.Parser

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.Strict.Lens as T
import Text.XML.Lens

import qualified Text.XML as XML

import Data.Function
       (fix)

import Network.URI.Lens.Extra
       (uriPathLens, _URI)

import Data.Aeson
       (Value)
import Data.Aeson.Lens

import Data.Time
       (Day, defaultTimeLocale, parseTimeM)

import Data.ByteString.Lazy
       (ByteString)

import qualified Text.HTML.DOM as DOM

-- Not sure of the lawfulness of this
_Document :: Prism' ByteString Document
_Document = prism' (XML.renderLBS XML.def) (Just . DOM.parseLBS)

title :: TextContentParser Element
title = contramap (foldOf (cosmos . named "title" . text)) textString

audibleNewReleases :: FeedParser Element
audibleNewReleases = fix $ \f -> FeedParser
  { entryParser = audibleNewReleasesEntry
  , entryLocator = EntryLocator (map (fixUrl (origin f)) . toListOf linkLens)
  , titleParser = title
  , origin = "https://www.audible.co.uk/newreleases"
  , slug = "audible-new-releases"
  }
  where
    -- hoo boy
    linkLens =
      cosmos . named "div" .  attributeIs "id" "product-list-a11y-skiplink-target" .
      cosmos . named "div" . attributeIs "data-widget" "productList" .
      cosmos . named "a" . attributeSatisfies "class" ("bc-link" `T.isPrefixOf`) . filtered (has (cosmos . named "div")) .
      attr "href" . T.unpacked
    fixUrl o u = o & set (_URI . uriPathLens) u

audibleNewReleasesEntry :: EntryParser Element
audibleNewReleasesEntry = fix $ \e -> EntryParser
  { entryTitleParser = title
  , entryUpdateParser = entryPublishedParser e
  , entryPublishedParser = contramap (preview dateLens) day
  , entryContentParser = contramap (preview contentLens) xhtmlContent
  }
  where
    -- The information is stored in a json
    dateLens =
      cosmos . named "div" . attributeIs "id" "bottom-0" ...
      named "script" . attributeIs "type" "application/ld+json" . text . _JSON @_ @Value .
        _Array . traversed .
        key "datePublished" . _String . T.unpacked . to (parseTimeM @Maybe @Day True defaultTimeLocale "%Y-%m-%d") .
        _Just
    contentLens =
      cosmos . named "div" . attributeIs "id" "bottom-0" ...
      named "script" . attributeIs "type" "application/ld+json" . text . _JSON @_ @Value .
        _Array . traversed .
        key "description" . _String .
        re T.utf8 . lazy .
        _Document . root .
        to XML.toXMLElement
