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
import           Hakyll.Web.Page
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
    Just "example.md"   @=? runRoutes routes (raw "example.md")
  where
    raw      = setVersion (Just "raw")
    expected =
        [ "example.md"
        , "russian.md"
        , raw "example.md"
        , raw "russian.md"
        , setVersion (Just "nav") "example.md"
        ]


--------------------------------------------------------------------------------
rules :: Rules ()
rules = do
    -- Compile some posts
    match "*.md" $ do
        route $ setExtension "html"
        compile pageCompiler

    -- Compile them, raw
    match "*.md" $ version "raw" $ do
        route idRoute
        compile getResourceString

    -- Regression test
    version "nav" $ match (fromList ["example.md"]) $ do
        route idRoute
        compile copyFileCompiler
