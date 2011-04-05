{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Identifier.Tests
    ( tests
    ) where

import Test.Framework
import Test.HUnit hiding (Test)

import Hakyll.Core.Identifier.Pattern
import TestSuite.Util

tests :: [Test]
tests = fromAssertions "capture"
    [ Just ["bar"]              @=? capture "foo/**" "foo/bar"
    , Just ["foo/bar"]          @=? capture "**" "foo/bar"
    , Nothing                   @=? capture "*" "foo/bar"
    , Just []                   @=? capture "foo" "foo"
    , Just ["foo"]              @=? capture "*/bar" "foo/bar"
    , Just ["foo/bar"]          @=? capture "**/qux" "foo/bar/qux"
    , Just ["foo/bar", "qux"]   @=? capture "**/*" "foo/bar/qux"
    , Just ["foo", "bar/qux"]   @=? capture "*/**" "foo/bar/qux"
    , Just ["foo"]              @=? capture "*.html" "foo.html"
    , Nothing                   @=? capture "*.html" "foo/bar.html"
    , Just ["foo/bar"]          @=? capture "**.html" "foo/bar.html"
    , Just ["foo/bar", "wut"]   @=? capture "**/qux/*" "foo/bar/qux/wut"
    , Just ["lol", "fun/large"] @=? capture "*cat/**.jpg" "lolcat/fun/large.jpg"
    ]
