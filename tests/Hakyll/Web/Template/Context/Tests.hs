--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Template.Context.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, (@=?))


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Provider
import           Hakyll.Core.Store              (Store)
import           Hakyll.Web.Template.Context
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Template.Context.Tests"
    [ testCase "testDateField" testDateField
    ]


--------------------------------------------------------------------------------
testDateField :: Assertion
testDateField = do
    store    <- newTestStore
    provider <- newTestProvider store

    date1 <- testContextDone store provider "example.md" "date" $
        dateField "date" "%B %e, %Y"
    date1 @=? "October 22, 2012"

    date2 <- testContextDone store provider
        "posts/2010-08-26-birthday.md" "date" $
            dateField "date" "%B %e, %Y"
    date2 @=? "August 26, 2010"

    cleanTestEnv


--------------------------------------------------------------------------------
testContextDone :: Store -> Provider -> Identifier -> String
                -> Context String -> IO String
testContextDone store provider identifier key context =
    testCompilerDone store provider identifier $ do
        item <- getResourceBody
        unContext context key item
