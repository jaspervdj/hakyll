--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
module Hakyll.Core.UnixFilter.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (testCase)
import qualified Test.Tasty.HUnit       as H


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.UnixFilter
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Core.UnixFilter.Tests" $
#ifdef mingw32_HOST_OS
    [] -- The `rev` utility is not present by default on Windows
#else
    [ testCase "unixFilter rev"   unixFilterRev ]
#endif
    ++
    [ testCase "unixFilter false" unixFilterFalse
    , testCase "unixFilter error" unixFilterError
    ]

testMarkdown :: Identifier
testMarkdown = "russian.md"

--------------------------------------------------------------------------------
#ifndef mingw32_HOST_OS
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
#endif


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
