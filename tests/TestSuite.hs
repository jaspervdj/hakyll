module TestSuite where

import Test.Framework (defaultMain, testGroup)

import qualified Hakyll.Core.DirectedGraph.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Hakyll.Core.DirectedGraph.Tests"
        Hakyll.Core.DirectedGraph.Tests.tests
    ]
