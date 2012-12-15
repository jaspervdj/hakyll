--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Rules.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.Set                       as S
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert, (@=?))


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Routes
import           Hakyll.Core.Rules
import           Hakyll.Core.Rules.Internal
import           Hakyll.Core.Writable.CopyFile
import           Hakyll.Web.Pandoc
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Rules.Tests"
    [ testCase "runRules" rulesTest
    ]


--------------------------------------------------------------------------------
rulesTest :: Assertion
rulesTest = withTestStore $ \store -> do
    provider <- newTestProvider store
    ruleSet  <- runRules rules provider
    let identifiers = S.fromList $ map fst $ rulesCompilers ruleSet
        routes      = rulesRoutes ruleSet

    -- Test that we have some identifiers and that the routes work out
    assert $ all (`S.member` identifiers) expected
    Just "example.html" @=? runRoutes routes "example.md"
    Just "example.md"   @=? runRoutes routes (sv "raw" "example.md")
    Just "example.md"   @=? runRoutes routes (sv "nav" "example.md")
  where
    sv g     = setVersion (Just g)
    expected =
        [ "example.md"
        , "russian.md"
        , sv "raw" "example.md"
        , sv "raw" "russian.md"
        , sv "nav" "example.md"
        ]


--------------------------------------------------------------------------------
rules :: Rules ()
rules = do
    -- Compile some posts
    match "*.md" $ do
        route $ setExtension "html"
        compile pandocCompiler

    -- Compile them, raw
    match "*.md" $ version "raw" $ do
        route idRoute
        compile getResourceString

    -- Regression test
    version "nav" $ match (fromList ["example.md"]) $ do
        route idRoute
        compile copyFileCompiler
