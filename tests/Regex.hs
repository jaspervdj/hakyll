module Regex 
    ( regexGroup
    ) where

import qualified Data.Map as M

import Data.Binary
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Text.Hakyll.Regex

-- Regex test group.
regexGroup = testGroup "Regex"
    [ testCase "test_splitRegex_1" test_splitRegex_1
    , testCase "test_splitRegex_2" test_splitRegex_2
    ]

-- Split Regex test cases.
test_splitRegex_1 = splitRegex "," "1,2,3" @?= ["1", "2", "3"]
test_splitRegex_2 = splitRegex "," ",1,2," @?= ["1", "2"]
