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
import           Data.Time.Format              (TimeLocale (..), defaultTimeLocale)
import           Data.Time.LocalTime           (TimeZone (..))
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

    let jst = defaultTimeLocale { knownTimeZones = [TimeZone (9 * 60) False "JST"] }
    date3 <- testContextDone store provider
        "posts/2016-08-02-localtime.md" "date" $
            dateFieldWith jst "date" "%Y-%m-%d %H:%M:%S %z"
    "2016-08-02 23:01:03 +0900" @=? date3

    cleanTestEnv


--------------------------------------------------------------------------------
testContextDone :: Store -> Provider -> Identifier -> String
                -> Context String -> IO String
testContextDone store provider identifier key context =
    testCompilerDone store provider identifier $ do
        item <- getResourceBody
        cf   <- unContext context key [] item
        case cf of
            StringField str -> return str
            ListField _ _   -> error $
                "Hakyll.Web.Template.Context.Tests.testContextDone: " ++
                "Didn't expect ListField"
