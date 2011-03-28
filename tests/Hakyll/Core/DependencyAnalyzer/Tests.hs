module Hakyll.Core.DependencyAnalyzer.Tests where

import Control.Arrow (second)
import qualified Data.Set as S
import Data.Monoid (mempty)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Hakyll.Core.DirectedGraph
import Hakyll.Core.DependencyAnalyzer

tests :: [Test]
tests =
    [ testCase "step [1]" step1
    , testCase "step [2]" step2
    ]

step1 :: Assertion
step1 = Just (S.fromList [1, 2, 5, 6, 7, 8, 9]) @?=
    stepAll (makeDependencyAnalyzer graph isOutOfDate prev)
  where
    node = curry $ second S.fromList

    graph = fromList
        [ node (8 :: Int) [2, 4, 6]
        , node 2 [4, 3]
        , node 4 [3]
        , node 6 [4]
        , node 3 []
        , node 9 [5]
        , node 5 [7]
        , node 1 [7]
        , node 7 []
        ]

    prev = fromList
        [ node 8 [2, 4, 6]
        , node 2 [4, 3]
        , node 4 [3]
        , node 6 [4]
        , node 3 []
        , node 9 [5]
        , node 5 [7]
        , node 1 [7]
        , node 7 [8]
        ]

    isOutOfDate = (`elem` [5, 2, 6])

step2 :: Assertion
step2 = Nothing @?= stepAll (makeDependencyAnalyzer graph isOutOfDate mempty)
  where
    node = curry $ second S.fromList

    -- Cycle: 4 -> 7 -> 5 -> 9 -> 4
    graph = fromList
        [ node (1 :: Int) [6]
        , node 2 [3]
        , node 3 []
        , node 4 [1, 7, 8]
        , node 5 [9]
        , node 6 [3]
        , node 7 [5]
        , node 8 [2]
        , node 9 [4]
        ]

    isOutOfDate = const True
