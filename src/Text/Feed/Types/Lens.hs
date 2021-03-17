{-# LANGUAGE TemplateHaskell #-}
module Text.Feed.Types.Lens where


import qualified Text.Feed.Types as F

import Control.Lens

makePrisms ''F.Feed
