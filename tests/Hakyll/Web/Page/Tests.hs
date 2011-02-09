{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Page.Tests
    ( tests
    ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Data.Map as M

import Hakyll.Web.Page
import Hakyll.Web.Page.Internal
import Hakyll.Web.Page.Read
import TestSuite.Util

tests :: [Test]
tests = fromAssertions "readPage"
    [ Page (M.singleton "foo" "bar") "body\n" @=? readPage
        "---        \n\
        \foo: bar   \n\
        \---        \n\
        \body"

    , Page M.empty "line one\nlijn twee\n" @=? readPage
        "line one\n\
        \lijn twee"

    , Page (M.fromList [("field1", "unos"), ("veld02", "deux")]) "" @=? readPage
        "---\n\
        \veld02: deux\n\
        \field1: unos\n\
        \---"
    ]
