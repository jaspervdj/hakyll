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
    [ testCase "solveDependencies [1]" solveDependencies1
    , testCase "filterObsolete [1]"    filterObsolete1
    , testCase "filterObsolete [2]"    filterObsolete2
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

solveDependencies1 :: Assertion
solveDependencies1 =  result == [3, 4, 2, 6, 8] || result == [3, 4, 2, 6, 8]
                    @? "solveDependencies1"
  where
    result = solveDependencies testGraph01

filterObsolete1 :: Assertion
filterObsolete1 =  nodes (filterObsolete [6] testGraph01) == S.fromList [6, 8]
                 @? "filterObsolete1"

filterObsolete2 :: Assertion
filterObsolete2 =
    nodes (filterObsolete [4] testGraph01) == S.fromList [4, 2, 6, 8]
        @? "filterObsolete2"
