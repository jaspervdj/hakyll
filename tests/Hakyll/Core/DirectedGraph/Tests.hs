module Hakyll.Core.DirectedGraph.Tests
    ( tests
    ) where

import Data.Set (Set)
import qualified Data.Set as S

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Hakyll.Core.DirectedGraph
import Hakyll.Core.DirectedGraph.DependencySolver
import Hakyll.Core.DirectedGraph.ObsoleteFilter

tests :: [Test]
tests =
    [ testCase "solveDependencies01" solveDependencies01
    , testCase "filterObsolete01"    filterObsolete01
    , testCase "filterObsolete02"    filterObsolete02
    ]

node :: Ord a => a -> [a] -> (a, Set a)
node t n = (t, S.fromList n)

testGraph01 :: DirectedGraph Int
testGraph01 = fromList
    [ node 8 [2, 4, 6]
    , node 2 [4, 3]
    , node 4 [3]
    , node 6 [4]
    , node 3 []
    ]

solveDependencies01 :: Assertion
solveDependencies01 =  result == [3, 4, 2, 6, 8] || result == [3, 4, 2, 6, 8]
                    @? "solveDependencies01"
  where
    result = solveDependencies testGraph01

filterObsolete01 :: Assertion
filterObsolete01 =  nodes (filterObsolete [6] testGraph01) == S.fromList [6, 8]
                 @? "filterObsolete01"

filterObsolete02 :: Assertion
filterObsolete02 =
    nodes (filterObsolete [4] testGraph01) == S.fromList [4, 2, 6, 8]
        @? "filterObsolete02"
