--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Runtime.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           System.FilePath           ((</>))
import           Test.Framework            (Test, testGroup)
import           Test.HUnit                (Assertion, (@?=))


--------------------------------------------------------------------------------
import           Hakyll.Core.Configuration
import           Hakyll.Core.Routes
import           Hakyll.Core.Rules
import           Hakyll.Core.Runtime
import           Hakyll.Web.Page
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Runtime.Tests" $ fromAssertions "run" [case01]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = withTestConfiguration $ \config -> do
    _ <- run config $ do
        match "*.md" $ do
            route   $ setExtension "html"
            compile $ pageCompiler

    out <- readFile $ destinationDirectory config </> "example.html"
    lines out @?=  ["<p>This is an example.</p>"]
