{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Routes.Tests
    ( tests
    ) where

import Test.Framework
import Test.HUnit hiding (Test)

import Hakyll.Core.Routes
import TestSuite.Util

tests :: [Test]
tests = fromAssertions "runRoutes"
    [ Just "foo.html" @=? runRoutes (setExtension "html") "foo"
    , Just "foo.html" @=? runRoutes (setExtension ".html") "foo"
    , Just "foo.html" @=? runRoutes (setExtension "html") "foo.markdown"
    , Just "foo.html" @=? runRoutes (setExtension ".html") "foo.markdown"
    ]
