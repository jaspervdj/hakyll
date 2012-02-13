{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Compiler.Tests
    ( tests
    ) where

import qualified Data.Map as M

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit as H

import Hakyll.Core.Compiler
import Hakyll.Core.Resource.Provider.Dummy
import Hakyll.Core.Util.Arrow
import TestSuite.Util

tests :: [Test]
tests =
    [ testCase "byExtension" byExtensionTest
    ]

byExtensionTest :: H.Assertion
byExtensionTest = do
    provider <- dummyResourceProvider $ M.empty
    txt      <- runCompilerJobTest compiler "foo.txt" provider uni
    css      <- runCompilerJobTest compiler "bar.css" provider uni
    html     <- runCompilerJobTest compiler "qux.html" provider uni
    H.assertEqual "byExtension" "txt" txt
    H.assertEqual "byExtension" "css" css
    H.assertEqual "byExtension" "unknown" html
  where
    uni      = ["foo.txt", "bar.css", "qux.html"]
    compiler = byExtension (constA ("unknown" :: String))
        [ (".txt", constA "txt")
        , (".css", constA "css")
        ]
