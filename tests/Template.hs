module Template where

import qualified Data.Map as M

import Test.QuickCheck
import Data.Binary

import Text.Hakyll.Internal.Template

-- Test encoding/decoding of templates.
prop_template_encode_id :: Template -> Bool
prop_template_encode_id template = decode (encode template) == template

-- Check we get the same sting with empty substitutions.
prop_substitute_id string =
    regularSubstitute (fromString string) M.empty == string

-- substitute test case 1.
prop_substitute_case1 string1 string2 =
    finalSubstitute template context == string1 ++ " costs $" ++ string2 ++ "."
  where
    template = fromString "$product costs $$$price."
    context = M.fromList [ ("product", string1)
                         , ("price", string2)
                         ]
