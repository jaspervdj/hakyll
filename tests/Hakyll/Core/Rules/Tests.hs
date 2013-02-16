--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Rules.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.IORef                     (IORef, newIORef, readIORef,
                                                 writeIORef)
import qualified Data.Set                       as S
import           Test.Framework                 (Test, testGroup)
import           Test.HUnit                     (Assertion, assert, (@=?))


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.File
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Routes
import           Hakyll.Core.Rules
import           Hakyll.Core.Rules.Internal
import           Hakyll.Web.Pandoc
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Rules.Tests" $ fromAssertions "runRules"
    [case01]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = do
    ioref    <- newIORef False
    store    <- newTestStore
    provider <- newTestProvider store
    ruleSet  <- runRules (rules01 ioref) provider
    let identifiers = S.fromList $ map fst $ rulesCompilers ruleSet
        routes      = rulesRoutes ruleSet

    -- Test that we have some identifiers and that the routes work out
    assert $ all (`S.member` identifiers) expected
    Just "example.html" @=? runRoutes routes "example.md"
    Just "example.md"   @=? runRoutes routes (sv "raw" "example.md")
    Just "example.md"   @=? runRoutes routes (sv "nav" "example.md")
    Just "example.mv1"  @=? runRoutes routes (sv "mv1" "example.md")
    Just "example.mv2"  @=? runRoutes routes (sv "mv2" "example.md")
    readIORef ioref >>= assert
    cleanTestEnv
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
rules01 :: IORef Bool -> Rules ()
rules01 ioref = do
    -- Compile some posts
    match "*.md" $ do
        route $ setExtension "html"
        compile pandocCompiler

    -- Yeah. I don't know how else to test this stuff?
    preprocess $ writeIORef ioref True

    -- Compile them, raw
    match "*.md" $ version "raw" $ do
        route idRoute
        compile getResourceString

    -- Regression test
    version "nav" $ match (fromList ["example.md"]) $ do
        route idRoute
        compile copyFileCompiler

    -- Another edge case: different versions in one match
    match "*.md" $ do
        version "mv1" $ do
            route $ setExtension "mv1"
            compile getResourceString
        version "mv2" $ do
            route $ setExtension "mv2"
            compile getResourceString
