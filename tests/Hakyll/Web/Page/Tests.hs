{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Page.Tests
    ( tests
    ) where

import Test.Framework
import Test.HUnit hiding (Test)

import qualified Data.Map as M

import Hakyll.Web.Page
import Hakyll.Web.Page.Read
import TestSuite.Util

tests :: [Test]
tests = fromAssertions "readPage"
    [ Page (M.singleton "foo" "bar") "body" @=? readPage
        "---        \n\
        \foo: bar   \n\
        \---        \n\
        \body"

    , Page M.empty "line one\nlijn twee" @=? readPage
        "line one\n\
        \lijn twee"

    , Page (M.fromList [("field1", "unos"), ("veld02", "deux")]) "" @=? readPage
        "---\n\
        \veld02: deux\n\
        \field1: unos\n\
        \---\n"

    , Page (M.fromList [("author", "jasper"), ("title", "lol")]) "O hai\n"
        @=? readPage
        "---\n\
        \author: jasper\n\
        \title: lol\n\
        \...\n\
        \O hai\n"
    ]
