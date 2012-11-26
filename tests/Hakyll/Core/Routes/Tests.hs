--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Routes.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework         (Test, testGroup)
import           Test.HUnit             ((@=?))


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Routes
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Routes.Tests" $ fromAssertions "runRoutes"
    [ Just "foo.html" @=? runRoutes (setExtension "html") "foo"
    , Just "foo.html" @=? runRoutes (setExtension ".html") "foo"
    , Just "foo.html" @=? runRoutes (setExtension "html") "foo.markdown"
    , Just "foo.html" @=? runRoutes (setExtension ".html") "foo.markdown"

    , Just "neve ro ddo reven" @=?
        runRoutes (customRoute (reverse . toFilePath  )) "never odd or even"

    , Just "foo" @=? runRoutes (constRoute "foo") "bar"

    , Just "tags/bar.xml" @=?
        runRoutes (gsubRoute "rss/" (const "")) "tags/rss/bar.xml"
    , Just "tags/bar.xml" @=?
        runRoutes (gsubRoute "rss/" (const "") `composeRoutes`
            setExtension "xml") "tags/rss/bar"
    ]
