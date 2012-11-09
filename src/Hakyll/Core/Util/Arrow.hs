--------------------------------------------------------------------------------
-- | Various arrow utility functions
module Hakyll.Core.Util.Arrow
    ( ArrowMap (..)
    , constA
    , sequenceA
    , unitA
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow (Arrow, ArrowChoice, arr, (&&&), (>>^))


--------------------------------------------------------------------------------
-- | Additional arrow typeclass for performance reasons.
class ArrowChoice a => ArrowMap a where
    mapA :: a b c -> a [b] [c]


--------------------------------------------------------------------------------
instance ArrowMap (->) where
    mapA = map


--------------------------------------------------------------------------------
constA :: Arrow a => c -> a b c
constA = arr . const


--------------------------------------------------------------------------------
sequenceA :: Arrow a => [a b c] -> a b [c]
sequenceA = foldr reduce $ constA []
  where
    reduce xa la = xa &&& la >>^ arr (uncurry (:))


--------------------------------------------------------------------------------
unitA :: Arrow a => a b ()
unitA = constA ()
