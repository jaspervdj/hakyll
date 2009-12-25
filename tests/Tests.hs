import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.HUnit

import Data.Char

import Text.Hakyll.CompressCSS
import Text.Hakyll.Util

main = defaultMain tests

tests = [ testGroup "Util group" [ testProperty "trim length" prop_trim_length
                                 , testProperty "trim id" prop_trim_id
                                 , testProperty "trim empty" prop_trim_empty
                                 , testCase "stripHTML 1" test_strip_html1
                                 , testCase "stripHTML 2" test_strip_html2
                                 , testCase "stripHTML 3" test_strip_html3
                                 , testCase "split 1" test_split1
                                 , testCase "split 2" test_split2
                                 ]

        , testGroup "CompressCSS group" [ testProperty "compressCSS length" prop_compress_css_length
                                        , testCase "compressCSS 1" test_compress_css1
                                        , testCase "compressCSS 2" test_compress_css2
                                        , testCase "compressCSS 3" test_compress_css3
                                        ]
        ]

-- Test that a string always becomes shorter when trimmed.
prop_trim_length str = length str >= length (trim str)

-- Check that a string which does not start or end with a space is not trimmed.
prop_trim_id str = (not $ null str)
                 && (not $ isSpace $ head str)
                 && (not $ isSpace $ last str)
                 ==> str == (trim str)

-- An string of only spaces should be reduced to an empty string.
prop_trim_empty str = (all isSpace str) ==> null (trim str)

-- Strip HTML test cases.
test_strip_html1 = stripHTML "<b>text</b>" @?= "text"
test_strip_html2 = stripHTML "text" @?= "text"
test_strip_html3 = stripHTML "<b>Hakyll</b> is an <i>awesome</i> web framework <img src=\"foo.png\" />" @?=
                             "Hakyll is an awesome web framework "

-- Split test cases.
test_split1 = split "," "1,2,3" @?= ["1", "2", "3"]
test_split2 = split "," ",1,2," @?= ["1", "2"]

-- CSS compression should always decrease the text length.
prop_compress_css_length str = length str >= length (compressCSS str)

-- Compress CSS test cases.
test_compress_css1 = compressCSS "a {  \n color  : red;  }" @?= "a{color:red}"
test_compress_css2 = compressCSS "img {border  :none;;;;  }" @?= "img{border:none}"
test_compress_css3 = compressCSS "p {font-size  : 90%;} h1 {color  :white;;;  }" @?= "p{font-size:90%}h1{color:white}"
