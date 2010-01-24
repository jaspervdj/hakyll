module Context 
    ( contextGroup
    ) where

import qualified Data.Map as M

import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit

import Text.Hakyll.Context

-- Context test group.
contextGroup = testGroup "Context"
    [ testCase "test_renderDate_1" test_renderDate_1
    , testCase "test_renderDate_2" test_renderDate_2
    , testCase "test_changeExtension_1" test_changeExtension_1
    ]

-- Date rendering test cases.
test_renderDate_1 =
    M.lookup "date" rendered @?= Just "December 30, 2009"
  where
    rendered = renderDate "date" "%B %e, %Y" "Unknown date"
                          (M.singleton "path" "2009-12-30-a-title.markdown")

test_renderDate_2 = M.lookup "date" rendered @?= Just "Unknown date"
  where
    rendered = renderDate "date" "%B %e, %Y" "Unknown date" $
                          M.singleton "path" "2009-badness-30-a-title.markdown"

-- changeExtension test cases.
test_changeExtension_1 = M.lookup "url" rendered @?= Just "foo.php"
  where
    rendered = changeExtension "php" (M.singleton "url" "foo.html")
