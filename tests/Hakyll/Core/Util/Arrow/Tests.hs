module Hakyll.Core.Util.Arrow.Tests
    ( tests
    ) where

import Test.Framework (Test)
import Test.HUnit ((@=?))

import Hakyll.Core.Util.Arrow
import TestSuite.Util

tests :: [Test]
tests = fromAssertions "sequenceA"
    [ [8, 20, 1] @=? sequenceA [(+ 4), (* 5), signum] (4 :: Int)
    ]
