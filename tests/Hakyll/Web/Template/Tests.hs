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
    [ applyTemplateAssertion readTemplate
        "bar" "$foo$" [("foo", "bar")]

    , applyTemplateAssertion readTemplate
        "$ barqux" "$$ $foo$$bar$" [("foo", "bar"), ("bar", "qux")]

    -- Hamlet templates
    , applyTemplateAssertion readHamletTemplate
        "<head><title>notice</title></head><body>A paragraph</body>"
        "<head\n\
        \    <title>#{title}\n\
        \<body\n\
        \    A paragraph\n"
        [("title", "notice")]
    ]

-- | Utility function to create quick template tests
--
applyTemplateAssertion :: (String -> Template)  -- ^ Template parser
                       -> String                -- ^ Expected
                       -> String                -- ^ Template
                       -> [(String, String)]    -- ^ Page
                       -> Assertion             -- ^ Resulting assertion
applyTemplateAssertion parser expected template page =
    expected @=? pageBody (applyTemplate (parser template)
                                         (fromMap $ M.fromList page))
