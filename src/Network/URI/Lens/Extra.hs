module Network.URI.Lens.Extra
  ( _URI
  , module Network.URI.Lens
  )
  where

import Control.Lens
import Network.URI
import Network.URI.Lens

_URI :: Prism' String URI
_URI = prism' (\u -> uriToString id u "") parseURI
