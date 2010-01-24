module Util where

import Data.Char

import Test.QuickCheck

import Text.Hakyll.Util

-- Test that a string always becomes shorter when trimmed.
prop_trim_length str = length str >= length (trim str)

-- Check that a string which does not start or end with a space is not trimmed.
prop_trim_id str = (not $ null str) && isAlreadyTrimmed ==> str == (trim str)
  where
    isAlreadyTrimmed :: Bool
    isAlreadyTrimmed = (not $ isSpace $ head str) && (not $ isSpace $ last str)

-- Check that a stripped string is shorter.
prop_stripHTML_length str = length str >= length (stripHTML str)

-- Check that strings without tags remain untouched.
prop_stripHTML_id str = (not $ any (`elem` ['>', '<']) str)
                      ==> str == stripHTML str
