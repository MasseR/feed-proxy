module Data.Trace where

import Data.Functor.Contravariant
import Data.Monoid (Ap(..))

newtype Trace m a = Trace { unTrace :: a -> m () }
  deriving Contravariant via Op (m ())
  deriving (Semigroup, Monoid) via (a -> Ap m ())

trace :: Trace m a -> a -> m ()
trace = unTrace
