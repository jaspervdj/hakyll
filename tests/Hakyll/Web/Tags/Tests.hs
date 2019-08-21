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

    noCategory <- testCategoryDone store provider "example.md"
    noCategory @?= [""]

    oneCategory1 <- testCategoryDone store provider "posts/2010-08-26-birthday.md"
    oneCategory1 @?= ["posts"]

    oneCategory2 <- testCategoryDone store provider "posts/2019/05/10/tomorrow.md"
    oneCategory2 @?= ["10"]

    cleanTestEnv

--------------------------------------------------------------------------------
testCategoryDone :: Store -> Provider -> Identifier -> IO [String]
testCategoryDone store provider identifier =
    testCompilerDone store provider identifier $ getCategory identifier
