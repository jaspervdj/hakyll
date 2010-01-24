module Template 
    ( templateGroup
    ) where

import qualified Data.Map as M

import Data.Binary
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit

import Text.Hakyll.Internal.Template

-- Template test group.
templateGroup = testGroup "Template"
    [ testProperty "prop_template_encode_id" prop_template_encode_id
    , testProperty "prop_substitute_id" prop_substitute_id
    , testCase "test_substitute_1" test_substitute_1
    , testCase "test_substitute_2" test_substitute_2
    ]

-- Test encoding/decoding of templates.
prop_template_encode_id :: Template -> Bool
prop_template_encode_id template = decode (encode template) == template

-- Check we get the same sting with empty substitutions.
prop_substitute_id string =
    regularSubstitute (fromString string) M.empty == string

-- substitute test case 1.
test_substitute_1 =
    finalSubstitute template context @?= "Banana costs $4."
  where
    template = fromString "$product costs $$$price."
    context = M.fromList [("product", "Banana"), ("price", "4")]

-- substitute test case 2.
test_substitute_2 =
    regularSubstitute template context @?= "$$root is a special key."
  where
    template = fromString "$$root is a special $thing."
    context = M.fromList [("root", "foo"), ("thing", "key")]
