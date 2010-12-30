-- | Various arrow utility functions
--
module Hakyll.Core.Util.Arrow
    ( sequenceArr
    , unitArr
    , withUnitArr
    ) where

import Prelude hiding (id)
import Control.Arrow (Arrow, (&&&), (>>>), arr, (***))
import Control.Category (id)

sequenceArr :: Arrow a
            => [a b c]
            -> a b [c]
sequenceArr = foldl reduce $ arr $ const []
  where
    reduce la xa = xa &&& la >>> arr (uncurry (:))

unitArr :: Arrow a
        => a b ()
unitArr = arr (const ())

withUnitArr :: Arrow a
            => a b  c
            -> a () d
            -> a b  (c, d)
withUnitArr a1 a2 = a1 &&& unitArr >>> id *** a2
