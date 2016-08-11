--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.UnixFilter.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.List                      (isInfixOf)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit                     as H


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Item
import           Hakyll.Core.Logger
import           Hakyll.Core.UnixFilter
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.UnixFilter.Tests"
    [ testCase "unixFilter rev"   unixFilterRev
    , testCase "unixFilter false" unixFilterFalse
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


--------------------------------------------------------------------------------
unixFilterFalse :: H.Assertion
unixFilterFalse = do
    store    <- newTestStore
    provider <- newTestProvider store
    result   <- testCompiler store provider "russian.md" compiler
    H.assert $ case result of
        CompilerError Error es -> any ("exit code" `isInfixOf`) es
        _                      -> False
    cleanTestEnv
  where
    compiler = getResourceString >>= withItemBody (unixFilter "false" [])
