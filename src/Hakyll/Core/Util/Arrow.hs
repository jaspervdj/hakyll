-- | Various arrow utility functions
--
module Hakyll.Core.Util.Arrow
    ( constA
    , sequenceA
    , unitA
    , mapA
    ) where

import Control.Arrow (Arrow, (&&&), arr, (>>^))

constA :: Arrow a
       => c
       -> a b c
constA = arr . const

sequenceA :: Arrow a
          => [a b c]
          -> a b [c]
sequenceA = foldl reduce $ constA []
  where
    reduce la xa = xa &&& la >>^ arr (uncurry (:))

unitA :: Arrow a
      => a b ()
unitA = constA ()

mapA :: Arrow a
     => (b -> c)
     -> a [b] [c]
mapA = arr . map
