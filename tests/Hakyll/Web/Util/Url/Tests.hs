module Hakyll.Web.Util.Url.Tests
    ( tests
    ) where

import Test.Framework
import Test.HUnit hiding (Test)

import Hakyll.Web.Util.Url
import TestSuite.Util

tests :: [Test]
tests = concat
    [ fromAssertions "toUrl"
        [ "/foo/bar.html"    @=? toUrl "foo/bar.html"
        , "/"                @=? toUrl "/"
        , "/funny-pics.html" @=? toUrl "/funny-pics.html"
        ]

    , fromAssertions "toSiteRoot"
        [ ".."    @=? toSiteRoot "/foo/bar.html"
        , "."     @=? toSiteRoot "index.html"
        , "."     @=? toSiteRoot "/index.html"
        , "../.." @=? toSiteRoot "foo/bar/qux"
        ]
    ]
