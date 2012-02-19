{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Identifier.Tests
    ( tests
    ) where

import Test.Framework
import Test.HUnit hiding (Test)

import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import TestSuite.Util

tests :: [Test]
tests = concat
    [ captureTests
    , matchesTests
    ]

captureTests :: [Test]
captureTests = fromAssertions "capture"
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
    , Just []                   @=? capture "\\*.jpg" "*.jpg"
    , Nothing                   @=? capture "\\*.jpg" "foo.jpg"
    ]

matchesTests :: [Test]
matchesTests = fromAssertions "matches"
    [ True  @=? matches (list ["foo.markdown"]) "foo.markdown"
    , False @=? matches (list ["foo"]) (Identifier (Just "foo") "foo")
    , True  @=? matches (regex "^foo/[^x]*$") "foo/bar"
    , False @=? matches (regex "^foo/[^x]*$") "foo/barx"
    , True  @=? matches (complement "foo.markdown") "bar.markdown"
    , False @=? matches (complement "foo.markdown") "foo.markdown"
    ]
