--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.UnixFilter.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit                     as H


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Core.UnixFilter
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.UnixFilter.Tests"
    [ testCase "unixFilter rev" unixFilterRev
    ]


--------------------------------------------------------------------------------
unixFilterRev :: H.Assertion
unixFilterRev = do
    store    <- newTestStore
    provider <- newTestProvider store
    output   <- testCompilerDone store provider "russian.md" compiler
    expected <- testCompilerDone store provider "russian.md" getResourceString
    H.assert $ rev (itemBody expected) == lines (itemBody output)
    cleanTestEnv
  where
    compiler = getResourceString >>= withItemBody (unixFilter "rev" [])
    rev      = map reverse . lines
