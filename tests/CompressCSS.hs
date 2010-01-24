module CompressCSS 
    ( compressCSSGroup
    ) where

import qualified Data.Map as M

import Data.Binary
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit

import Text.Hakyll.Internal.CompressCSS

-- CompressCSS test group.
compressCSSGroup = testGroup "CompressCSS"
    [ testProperty "prop_compressCSS_length" prop_compressCSS_length
    , testCase "test_compressCSS_1" test_compressCSS_1
    , testCase "test_compressCSS_2" test_compressCSS_2
    , testCase "test_compressCSS_3" test_compressCSS_3
    , testCase "test_compressCSS_4" test_compressCSS_4
    ]

-- CSS compression should always decrease the text length.
prop_compressCSS_length str = length str >= length (compressCSS str)

-- Compress CSS test cases.
test_compressCSS_1 = compressCSS "a {  \n color  : red;  }" @?= "a{color:red}"
test_compressCSS_2 = compressCSS "img {border  :none;;;;  }"
                   @?= "img{border:none}"
test_compressCSS_3 =
    compressCSS "p {font-size  : 90%;} h1 {color  :white;;;  }"
    @?= "p{font-size:90%}h1{color:white}"
test_compressCSS_4 = compressCSS "a { /* /* red is pretty cool */ color: red; }"
                   @?= "a{color:red}"
