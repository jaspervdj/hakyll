--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Store.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.Typeable                        (typeOf)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit                           as H
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


--------------------------------------------------------------------------------
import qualified Hakyll.Core.Store                    as Store
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testProperty "simple get . set"     simpleSetGet
    , testProperty "persistent get . set" persistentSetGet
    , testCase     "WrongType get . set"  wrongType
    ]


--------------------------------------------------------------------------------
simpleSetGet :: Property
simpleSetGet = monadicIO $ do
    key   <- pick arbitrary
    value <- pick arbitrary
    store <- run $ makeStoreTest
    run $ Store.set store key (value :: String)
    value' <- run $ Store.get store key
    assert $ Store.Found value == value'


--------------------------------------------------------------------------------
persistentSetGet :: Property
persistentSetGet = monadicIO $ do
    key    <- pick arbitrary
    value  <- pick arbitrary
    store1 <- run $ makeStoreTest
    run $ Store.set store1 key (value :: String)
    -- Now Create another store from the same dir to test persistence
    store2 <- run $ makeStoreTest
    value' <- run $ Store.get store2 key
    assert $ Store.Found value == value'


--------------------------------------------------------------------------------
wrongType :: H.Assertion
wrongType = do
    store <- makeStoreTest
    -- Store a string and try to fetch an int
    Store.set store ["foo", "bar"] ("qux" :: String)
    value <- Store.get store ["foo", "bar"] :: IO (Store.Result Int)
    print value
    H.assert $ case value of
        Store.WrongType e t ->
            e == typeOf (undefined :: Int) &&
            t == typeOf (undefined :: String)
        _                   -> False
