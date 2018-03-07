--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Provider.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider
import           Test.Tasty           (TestTree, testGroup)
import           Test.Tasty.HUnit     (Assertion, testCase, (@=?))
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Core.Provider.Tests"
    [ testCase "case01" case01
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = do
    store    <- newTestStore
    provider <- newTestProvider store
    True @=? resourceExists provider "example.md"

    metadata <- resourceMetadata provider "example.md"
    Just "An example"    @=? lookupString "title"    metadata
    Just "External data" @=? lookupString "external" metadata

    doesntExist <- resourceMetadata provider "doesntexist.md"
    mempty @=? doesntExist
    cleanTestEnv
