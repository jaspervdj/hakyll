import Data.Char
import qualified Data.Map as M

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.HUnit

import Text.Hakyll.CompressCSS
import Text.Hakyll.Util
import Text.Hakyll.Regex
import Text.Hakyll.Context
import Text.Hakyll.File

main = defaultMain tests

tests = [ testGroup "Util group"
            [ testProperty "trim length" prop_trim_length
            , testProperty "trim id" prop_trim_id
            , testProperty "trim empty" prop_trim_empty
            , testCase "stripHTML 1" test_strip_html1
            , testCase "stripHTML 2" test_strip_html2
            , testCase "stripHTML 3" test_strip_html3
            , testCase "link 1" test_link1
            , testCase "link 2" test_link2
            ]

        , testGroup "Regex group"
            [ testCase "splitRegex 1" test_split_regex1
            , testCase "splitRegex 2" test_split_regex2
            ]

        , testGroup "CompressCSS group" 
            [ testProperty "compressCSS length" prop_compress_css_length
            , testCase "compressCSS 1" test_compress_css1
            , testCase "compressCSS 2" test_compress_css2
            , testCase "compressCSS 3" test_compress_css3
            , testCase "compressCSS 4" test_compress_css4
            ]

        , testGroup "Context group"
            [ testCase "renderDate 1" test_render_date1
            , testCase "renderDate 2" test_render_date1
            ]

        , testGroup "File group"
            [ testCase "toRoot 1" test_to_root1
            , testCase "toRoot 2" test_to_root2
            , testCase "toRoot 3" test_to_root3
            , testCase "removeSpaces 1" test_remove_spaces1
            , testCase "removeSpaces 2" test_remove_spaces2
            , testProperty "havingExtension count" prop_having_extension_count
            , testCase "havingExtension 1" test_having_extension1
            , testCase "havingExtension 2" test_having_extension2
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
test_strip_html3 =
    stripHTML "<b>Hakyll</b>, a <i>website</i> generator<img src=\"foo.png\" />"
        @?= "Hakyll, a website generator"

-- Link test cases.
test_link1 = link "foo bar" "/foo/bar.html"
           @?= "<a href=\"/foo/bar.html\">foo bar</a>"
test_link2 = link "back home" "/" @?= "<a href=\"/\">back home</a>"

-- Split Regex test cases.
test_split_regex1 = splitRegex "," "1,2,3" @?= ["1", "2", "3"]
test_split_regex2 = splitRegex "," ",1,2," @?= ["1", "2"]

-- CSS compression should always decrease the text length.
prop_compress_css_length str = length str >= length (compressCSS str)

-- Compress CSS test cases.
test_compress_css1 = compressCSS "a {  \n color  : red;  }" @?= "a{color:red}"
test_compress_css2 = compressCSS "img {border  :none;;;;  }"
                   @?= "img{border:none}"
test_compress_css3 =
    compressCSS "p {font-size  : 90%;} h1 {color  :white;;;  }"
    @?= "p{font-size:90%}h1{color:white}"
test_compress_css4 = compressCSS "a { /* /* red is pretty cool */ color: red; }"
                   @?= "a{color:red}"

-- Date rendering test cases.
test_render_date1 =
    M.lookup "date" rendered @?= Just "December 30, 2009"
  where
    rendered = renderDate "date" "%B %e, %Y" "Unknown date"
                          (M.singleton "path" "2009-12-30-a-title.markdown")

test_render_date2 = M.lookup "date" rendered @?= Just "Unknown date"
  where
    rendered = renderDate "date" "%B %e, %Y" "Unknown date" $
                          M.singleton "path" "2009-badness-30-a-title.markdown"

-- toRoot test cases
test_to_root1 = toRoot "/posts/foo.html" @?= ".."
test_to_root2 = toRoot "posts/foo.html" @?= ".."
test_to_root3 = toRoot "foo.html" @?= "."

-- removeSpaces test cases
test_remove_spaces1 = removeSpaces "$root/tags/random crap.html"
                    @?= "$root/tags/random-crap.html"
test_remove_spaces2 = removeSpaces "another simple example.zip"
                    @?= "another-simple-example.zip"

-- Add an extension, and test that they have that extension
prop_having_extension_count names extension =
    not (any ('.' `elem`) names || any (`elem` extension) "./\\")
    ==> havingExtension fullExtension withExtensions == withExtensions
  where
    fullExtension = '.' : extension
    withExtensions = map (++ fullExtension) names

-- Having extension test cases
test_having_extension1 = havingExtension ".foo" ["file.bar", "file.txt"] @?= []
test_having_extension2 = havingExtension ".foo" ["file.foo", "file.txt"]
                       @?= ["file.foo"]
