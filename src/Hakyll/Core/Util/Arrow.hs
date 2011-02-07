-- | Various arrow utility functions
--
module Hakyll.Core.Util.Arrow
    ( constA
    , sequenceA
    , unitA
    , mapA
    ) where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ( Arrow, ArrowChoice, (&&&), arr, (>>^), (|||)
                     , (>>>), (***)
                     )

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

mapA :: ArrowChoice a
     => a b c
     -> a [b] [c]
mapA f = arr listEither >>> id ||| (f *** mapA f >>> arr (uncurry (:)))
  where
    listEither []       = Left []
    listEither (x : xs) = Right (x, xs)
