--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Feed.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, testCase)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Provider
import           Hakyll.Web.Feed
import           Hakyll.Web.Template.Context
import           TestSuite.Util
--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Web.Feed.Tests"
    [ testCase "validateSucceeds" validateSucceeds
    , testCase "validateFails" validateFails
    ]

validateFails :: Assertion
validateFails = do
    store <- newTestStore
    provider <- newTestProvider store

    item <- testCompilerDone store provider "example.md"
      $ makeItem (resourceFilePath provider "example.md")

    testCompilerError store provider "feed.xml"
      (renderRss feedConfiguration ctx [item])
      "Generated feed contains invalid XML (perhaps you id not escape a metadata field?)"
    cleanTestEnv
  where
    title = "invalid & not xml"
    ctx = constField "title" title `mappend`
      constField "description" "" `mappend`
      defaultContext

validateSucceeds :: Assertion
validateSucceeds = do
    store <- newTestStore
    provider <- newTestProvider store

    item <- testCompilerDone store provider "example.md"
      $ makeItem (resourceFilePath provider "example.md")

    _ <- testCompilerDone store provider "feed.xml"
      $ renderRss feedConfiguration ctx [item]
    cleanTestEnv
  where
    title = "valid xml"
    ctx = constField "title" title `mappend`
      constField "description" "" `mappend`
      defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "test"
  , feedDescription = "test"
  , feedAuthorName = "test"
  , feedAuthorEmail = ""
  , feedRoot = "example.org"
  }
