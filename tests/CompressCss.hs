-- | Module testing @Text.Hakyll.Internal.CompressCss@.
module CompressCss 
    ( compressCssGroup
    ) where

import qualified Data.Map as M

import Data.Binary
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit

import Text.Hakyll.Internal.CompressCss

-- CompressCss test group.
compressCssGroup = testGroup "CompressCss"
    [ testProperty "prop_compressCss_length" prop_compressCss_length
    , testCase "test_compressCss_1" test_compressCss_1
    , testCase "test_compressCss_2" test_compressCss_2
    , testCase "test_compressCss_3" test_compressCss_3
    , testCase "test_compressCss_4" test_compressCss_4
    ]

-- | Css compression should always decrease the text length.
prop_compressCss_length str = length str >= length (compressCss str)

-- | compressCss test case 1.
test_compressCss_1 = compressCss "a {  \n color  : red;  }" @?= "a{color:red}"

-- | compressCss test case 2.
test_compressCss_2 = compressCss "img {border  :none;;;;  }"
                   @?= "img{border:none}"

-- | compressCss test case 3.
test_compressCss_3 =
    compressCss "p {font-size  : 90%;} h1 {color  :white;;;  }"
    @?= "p{font-size:90%}h1{color:white}"

-- | compressCss test case 4.
test_compressCss_4 = compressCss "a { /* /* red is pretty cool */ color: red; }"
                   @?= "a{color:red}"
