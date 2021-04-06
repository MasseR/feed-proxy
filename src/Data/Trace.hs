module Data.Trace where

import Data.Functor.Contravariant

newtype Trace m a = Trace { unTrace :: a -> m () }

deriving via (Op (m ())) instance Contravariant (Trace m)

trace :: Trace m a -> a -> m ()
trace = unTrace
