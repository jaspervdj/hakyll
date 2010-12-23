module TestSuite where

import Test.Framework (defaultMain, testGroup)

import qualified Hakyll.Core.DirectedGraph.Tests
import qualified Hakyll.Core.Identifier.Tests
import qualified Hakyll.Core.Route.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Hakyll.Core.DirectedGraph.Tests"
        Hakyll.Core.DirectedGraph.Tests.tests
    , testGroup "Hakyll.Core.Identifier.Tests"
        Hakyll.Core.Identifier.Tests.tests
    , testGroup "Hakyll.Core.Route.Tests"
        Hakyll.Core.Route.Tests.tests
    ]
