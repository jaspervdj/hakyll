module Page 
    ( pageGroup
    ) where

import qualified Data.Map as M

import Data.Binary
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit

import Text.Hakyll.Page

-- Page test group.
pageGroup = testGroup "Page"
    [ testProperty "prop_page_encode_id" prop_page_encode_id
    ]

-- Test encoding/decoding of pages.
prop_page_encode_id :: Page -> Bool
prop_page_encode_id page = decode (encode page) == page
