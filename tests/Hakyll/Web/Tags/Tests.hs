--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Tags.Tests
    ( tests
    ) where

--------------------------------------------------------------------------------
import           Test.Tasty                  (TestTree, testGroup)
import           Test.Tasty.HUnit            (Assertion, testCase, (@?=))

--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Provider
import           Hakyll.Core.Store           (Store)
import           Hakyll.Web.Tags
import           TestSuite.Util

tests :: TestTree
tests = testGroup "Hakyll.Web.Tags"
    [ testCase "testGetCategory" testGetCategory
    ]

testGetCategory :: Assertion
testGetCategory = do
    store    <- newTestStore
    provider <- newTestProvider store

    noCategory1 <- testCategoryDone store provider "example.md"
    noCategory1 @?= []

    noCategory2 <- testCategoryDone store provider "posts/2010-08-26-birthday.md"
    noCategory2 @?= []

    severalCategories <- testCategoryDone store provider "posts/2019/05/10/tomorrow.md"
    severalCategories @?= ["2019","05","10"]

    cleanTestEnv

--------------------------------------------------------------------------------
testCategoryDone :: Store -> Provider -> Identifier -> IO [String]
testCategoryDone store provider identifier =
    testCompilerDone store provider identifier $ getCategory identifier
