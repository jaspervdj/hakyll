--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Routes.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework         (Test, testGroup)
import           Test.HUnit             (Assertion, (@=?))


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Routes
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Routes.Tests" $ fromAssertions "runRoutes"
    [ testRoutes "foo.html" (setExtension "html") "foo"
    , testRoutes "foo.html" (setExtension ".html") "foo"
    , testRoutes "foo.html" (setExtension "html") "foo.markdown"
    , testRoutes "foo.html" (setExtension ".html") "foo.markdown"

    , testRoutes "neve ro ddo reven"
        (customRoute (reverse . toFilePath  )) "never odd or even"

    , testRoutes "foo" (constRoute "foo") "bar"

    , testRoutes "tags/bar.xml" (gsubRoute "rss/" (const "")) "tags/rss/bar.xml"
    , testRoutes "tags/bar.xml"
        (gsubRoute "rss/" (const "") `composeRoutes` setExtension "xml")
        "tags/rss/bar"
    ]


--------------------------------------------------------------------------------
testRoutes :: FilePath -> Routes -> Identifier -> Assertion
testRoutes expected r id' = do
    route <- runRoutes r (error "Hakyll.Core.Routes.Tests: no provider") id'
    Just expected @=? route
