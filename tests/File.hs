module File 
    ( fileGroup
    ) where

import qualified Data.Map as M

import Data.Binary
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Text.Hakyll.File

-- File test group.
fileGroup = testGroup "File"
    [ testCase "test_toRoot_1" test_toRoot_1
    , testCase "test_toRoot_2" test_toRoot_2
    , testCase "test_toRoot_3" test_toRoot_3
    , testCase "test_removeSpaces_1" test_removeSpaces_1
    , testCase "test_removeSpaces_2" test_removeSpaces_2
    , testCase "test_havingExtension_1" test_havingExtension_1
    , testCase "test_havingExtension_2" test_havingExtension_2
    ]


-- toRoot test cases
test_toRoot_1 = toRoot "/posts/foo.html" @?= ".."
test_toRoot_2 = toRoot "posts/foo.html" @?= ".."
test_toRoot_3 = toRoot "foo.html" @?= "."

-- removeSpaces test cases
test_removeSpaces_1 = removeSpaces "$root/tags/random crap.html"
                    @?= "$root/tags/random-crap.html"
test_removeSpaces_2 = removeSpaces "another simple example.zip"
                    @?= "another-simple-example.zip"

-- Having extension test cases
test_havingExtension_1 = havingExtension ".foo" ["file.bar", "file.txt"] @?= []
test_havingExtension_2 = havingExtension ".foo" ["file.foo", "file.txt"]
                       @?= ["file.foo"]
