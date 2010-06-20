module Template 
    ( templateGroup
    ) where

import qualified Data.Map as M
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.Monoid (mempty)

import Data.Binary
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Text.Hakyll.Context (Context (..))
import Text.Hakyll.Internal.Template

-- Template test group.
templateGroup = testGroup "Template"
    [ testProperty "prop_template_encode_id" prop_template_encode_id
    , testProperty "prop_substitute_id" prop_substitute_id
    , testCase "test_substitute_1" test_substitute_1
    , testCase "test_substitute_2" test_substitute_2
    ]

-- | Generate arbitrary templates from a given length.
arbitraryTemplate :: Int -> Gen Template
arbitraryTemplate 0 = return End
arbitraryTemplate length' = oneof [ do chunk <- chunk'
                                       Chunk chunk <$> template'
                                  , do key <- key'
                                       Identifier key <$> template'
                                  , EscapeCharacter <$> template'
                                  ]
  where
    template' = arbitraryTemplate (length' - 1)
    -- Generate keys.
    key' = do l <- choose (5, 10)
              replicateM l $ choose ('a', 'z')
    -- Generate non-empty chunks.
    chunk' = do string <- arbitrary
                let sanitized = filter (/= '$') string
                return $ if null sanitized then "foo"
                                           else sanitized

-- | Make @Template@ testable.
instance Arbitrary Template where
    arbitrary = choose (0, 20) >>= arbitraryTemplate

    shrink (Chunk chunk template) = [template, Chunk chunk End]
    shrink (Identifier key template) = [template, Identifier key End]
    shrink (EscapeCharacter template) = [template, EscapeCharacter End]
    shrink End = []

-- Test encoding/decoding of templates.
prop_template_encode_id :: Template -> Bool
prop_template_encode_id template = decode (encode template) == template

-- Check we get the same sting with empty substitutions.
prop_substitute_id string =
    regularSubstitute (fromString string) mempty == string

-- substitute test case 1.
test_substitute_1 =
    finalSubstitute template context @?= "Banana costs $4."
  where
    template = fromString "$product costs $$$price."
    context = Context $ M.fromList [("product", "Banana"), ("price", "4")]

-- substitute test case 2.
test_substitute_2 =
    regularSubstitute template context @?= "$$root is a special key."
  where
    template = fromString "$$root is a special $thing."
    context = Context $ M.fromList [("root", "foo"), ("thing", "key")]
