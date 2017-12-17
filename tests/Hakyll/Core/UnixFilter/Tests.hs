--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.UnixFilter.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.List                     (isInfixOf)
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit              (testCase)
import qualified Test.Tasty.HUnit              as H


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
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
    H.assert $ rev (itemBody expected) == lines (itemBody output)
    cleanTestEnv
  where
    compiler = getResourceString >>= withItemBody (unixFilter "rev" [])
    rev      = map reverse . lines


--------------------------------------------------------------------------------
unixFilterFalse :: H.Assertion
unixFilterFalse = do
    store    <- newTestStore
    provider <- newTestProvider store
    result   <- testCompiler store provider testMarkdown compiler
    H.assert $ case result of
        CompilerError es -> any ("exit code" `isInfixOf`) es
        _                -> False
    cleanTestEnv
  where
    compiler = getResourceString >>= withItemBody (unixFilter "false" [])


--------------------------------------------------------------------------------
unixFilterError :: H.Assertion
unixFilterError = do
    store    <- newTestStore
    provider <- newTestProvider store
    result   <- testCompiler store provider testMarkdown compiler
    H.assert $ case result of
        CompilerError es -> any ("invalid option" `isInfixOf`) es
        _                -> False
    cleanTestEnv
  where
    compiler = getResourceString >>= withItemBody (unixFilter "head" ["-#"])