{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Template.Tests
    ( tests
    ) where

import Test.Framework
import Test.HUnit hiding (Test)

import qualified Data.Map as M

import Hakyll.Web.Page
import Hakyll.Web.Template
import Hakyll.Web.Template.Read
import TestSuite.Util

tests :: [Test]
tests = fromAssertions "applyTemplate"
    -- Hakyll templates
    [ applyTemplateAssertion readTemplate applyTemplate
        "bar" "$foo$" [("foo", "bar")]

    , applyTemplateAssertion readTemplate applyTemplate
        "$ barqux" "$$ $foo$$bar$" [("foo", "bar"), ("bar", "qux")]

    , applyTemplateAssertion readTemplate applyTemplate
        "$foo$" "$foo$" []

    -- Hamlet templates
    , applyTemplateAssertion readHamletTemplate applyTemplate
        "<head><title>notice</title></head><body>A paragraph</body>"
        "<head>\n\
        \    <title>#{title}\n\
        \<body>\n\
        \    A paragraph\n"
        [("title", "notice")]

    -- Missing keys
    , let missing "foo" = "bar"
          missing "bar" = "qux"
          missing x     = reverse x
      in applyTemplateAssertion readTemplate (applyTemplateWith missing)
        "bar foo ver" "$foo$ $bar$ $rev$" [("bar", "foo")]
    ]

-- | Utility function to create quick template tests
--
applyTemplateAssertion :: (String -> Template)
                       -> (Template -> Page String -> Page String)
                       -> String
                       -> String
                       -> [(String, String)]
                       -> Assertion
applyTemplateAssertion parser apply expected template page =
    expected @=? pageBody (apply (parser template) (fromMap $ M.fromList page))
