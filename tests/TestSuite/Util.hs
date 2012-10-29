-- | Test utilities
--
module TestSuite.Util
    ( fromAssertions
    , makeStoreTest
    , runCompilerJobTest
    ) where

import Data.Monoid (mempty)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Identifier
import Hakyll.Core.Logger
import Hakyll.Core.Resource.Provider
import Hakyll.Core.Store (Store)
import qualified Hakyll.Core.Store as Store

fromAssertions :: String       -- ^ Name
               -> [Assertion]  -- ^ Cases
               -> [Test]       -- ^ Result tests
fromAssertions name = zipWith testCase names
  where
    names = map (\n -> name ++ " [" ++ show n ++ "]") [1 :: Int ..]

-- | Create a store for testing
--
makeStoreTest :: IO Store
makeStoreTest = Store.new True "_store"

-- | Testing for 'runCompilerJob'
--
runCompilerJobTest :: Compiler () a
                   -> Identifier ()
                   -> ResourceProvider
                   -> [Identifier ()]
                   -> IO a
runCompilerJobTest compiler id' provider uni = do
    store <- makeStoreTest
    logger <- makeLogger $ const $ return ()
    Right x <- runCompilerJob compiler id' provider uni mempty store True logger
    return x
