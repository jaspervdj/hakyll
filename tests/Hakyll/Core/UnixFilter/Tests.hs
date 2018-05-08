--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.UnixFilter.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit              (testCase)
import qualified Test.Tasty.HUnit              as H


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Core.UnixFilter
import           Hakyll.Core.Identifier
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Core.UnixFilter.Tests"
    [ testCase "unixFilter rev"   unixFilterRev
    , testCase "unixFilter false" unixFilterFalse
    , testCase "unixFilter error" unixFilterError
    ]

testMarkdown :: Identifier
testMarkdown = "russian.md"

--------------------------------------------------------------------------------
unixFilterRev :: H.Assertion
unixFilterRev = do
    store    <- newTestStore
    provider <- newTestProvider store
    output   <- testCompilerDone store provider testMarkdown compiler
    expected <- testCompilerDone store provider testMarkdown getResourceString
    rev (itemBody expected) H.@=? lines (itemBody output)
    cleanTestEnv
  where
    compiler = getResourceString >>= withItemBody (unixFilter "rev" [])
    rev      = map reverse . lines


--------------------------------------------------------------------------------
unixFilterFalse :: H.Assertion
unixFilterFalse = do
    store    <- newTestStore
    provider <- newTestProvider store
    testCompilerError store provider testMarkdown compiler "exit code"
    cleanTestEnv
  where
    compiler = getResourceString >>= withItemBody (unixFilter "false" [])


--------------------------------------------------------------------------------
unixFilterError :: H.Assertion
unixFilterError = do
    store    <- newTestStore
    provider <- newTestProvider store
    testCompilerError store provider testMarkdown compiler "option"
    cleanTestEnv
  where
    compiler = getResourceString >>= withItemBody (unixFilter "head" ["-#"])
