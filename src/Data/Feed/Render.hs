module Data.Feed.Render (render) where

import Data.Set
       (Set)

import Data.Text
       (Text)

import Data.ByteString.Lazy
       (ByteString)

import qualified Text.Feed.Types as F

import qualified Text.XML as XML

import Text.XML
       (Document(..), Prologue(..))

import Text.Atom.Feed
       (Feed(..))

import Text.Feed.Export
       (xmlFeed)

render :: Feed -> Either (Set Text) ByteString
render = render' . F.AtomFeed
  where
    render' :: F.Feed -> Either (Set Text) ByteString
    render' = fmap (\e -> XML.renderLBS XML.def{XML.rsPretty = True} (Document (Prologue [] Nothing []) e [])) . XML.fromXMLElement . xmlFeed
