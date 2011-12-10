-- | Various arrow utility functions
--
module Hakyll.Core.Util.Arrow
    ( constA
    , sequenceA
    , unitA
    ) where

import Control.Arrow (Arrow, (&&&), arr, (>>^))

constA :: Arrow a
       => c
       -> a b c
constA = arr . const

sequenceA :: Arrow a
          => [a b c]
          -> a b [c]
sequenceA = foldr reduce $ constA []
  where
    reduce xa la = xa &&& la >>^ arr (uncurry (:))

unitA :: Arrow a
      => a b ()
unitA = constA ()
