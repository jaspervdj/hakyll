--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Provider.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.Map                       as M
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert, (@=?))


--------------------------------------------------------------------------------
import           Hakyll.Core.Provider
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Provider.Tests"
    [ testCase "case01" case01
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = do
    store    <- newTestStore
    provider <- newTestProvider store
    assert $ resourceExists provider "example.md"

    metadata <- resourceMetadata provider "example.md"
    Just "An example"    @=? M.lookup "title"    metadata
    Just "External data" @=? M.lookup "external" metadata

    doesntExist <- resourceMetadata provider "doesntexist.md"
    M.empty @=? doesntExist
