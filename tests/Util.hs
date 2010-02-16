module Util 
    ( utilGroup
    ) where

import Data.Char

import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit

import Text.Hakyll.Util

-- Util test group.
utilGroup = testGroup "Util"
    [ testProperty "prop_trim_length" prop_trim_length
    , testProperty "prop_trim_id" prop_trim_id
    , testProperty "prop_stripHtml_length" prop_stripHtml_length
    , testProperty "prop_stripHtml_id" prop_stripHtml_id
    , testCase "test_stripHtml_1" test_stripHtml_1
    , testCase "test_stripHtml_2" test_stripHtml_2
    , testCase "test_stripHtml_3" test_stripHtml_3
    , testCase "test_link_1" test_link_1
    , testCase "test_link_2" test_link_2
    ]

-- Test that a string always becomes shorter when trimmed.
prop_trim_length str = length str >= length (trim str)

-- Check that a string which does not start or end with a space is not trimmed.
prop_trim_id str = (not $ null str) && isAlreadyTrimmed ==> str == (trim str)
  where
    isAlreadyTrimmed :: Bool
    isAlreadyTrimmed = (not $ isSpace $ head str) && (not $ isSpace $ last str)

-- Check that a stripped string is shorter.
prop_stripHtml_length str = length str >= length (stripHtml str)

-- Check that strings without tags remain untouched.
prop_stripHtml_id str = (not $ any (`elem` ['>', '<']) str)
                      ==> str == stripHtml str

-- Strip Html test cases.
test_stripHtml_1 = stripHtml "<b>text</b>" @?= "text"
test_stripHtml_2 = stripHtml "text" @?= "text"
test_stripHtml_3 =
    stripHtml "<b>Hakyll</b>, a <i>website</i> generator<img src=\"foo.png\" />"
        @?= "Hakyll, a website generator"

-- Link test cases.
test_link_1 = link "foo bar" "/foo/bar.html"
           @?= "<a href=\"/foo/bar.html\">foo bar</a>"
test_link_2 = link "back home" "/" @?= "<a href=\"/\">back home</a>"
