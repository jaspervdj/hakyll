module Hakyll.Web.Util.Html.Tests
    ( tests
    ) where

import Test.Framework
import Test.HUnit hiding (Test)

import Hakyll.Web.Util.Html
import TestSuite.Util

tests :: [Test]
tests = concat
    [ fromAssertions "stripTags"
        [ "foo"     @=? stripTags "<p>foo</p>"
        , "foo bar" @=? stripTags "<p>foo</p> bar"
        , "foo"     @=? stripTags "<p>foo</p"
        ]
    ]
