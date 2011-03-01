-- | Test utilities
--
module TestSuite.Util
    ( fromAssertions
    ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

fromAssertions :: String       -- ^ Name
               -> [Assertion]  -- ^ Cases
               -> [Test]       -- ^ Result tests
fromAssertions name = zipWith testCase names
  where
    names = map (\n -> name ++ " [" ++ show n ++ "]") [1 :: Int ..]
