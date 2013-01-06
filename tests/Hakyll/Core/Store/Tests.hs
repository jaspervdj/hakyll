--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Store.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.Typeable                        (typeOf)
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.HUnit                           as H
import qualified Test.QuickCheck                      as Q
import qualified Test.QuickCheck.Monadic              as Q


--------------------------------------------------------------------------------
import qualified Hakyll.Core.Store                    as Store
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Store.Tests"
    [ testProperty "simple get . set"     simpleSetGet
    , testProperty "persistent get . set" persistentSetGet
    , testCase     "WrongType get . set"  wrongType
    ]


--------------------------------------------------------------------------------
simpleSetGet :: Q.Property
simpleSetGet = Q.monadicIO $ do
    key   <- Q.pick Q.arbitrary
    value <- Q.pick Q.arbitrary
    store <- Q.run newTestStore
    Q.run $ Store.set store key (value :: String)
    value' <- Q.run $ Store.get store key
    Q.assert $ Store.Found value == value'
    Q.run cleanTestEnv


--------------------------------------------------------------------------------
persistentSetGet :: Q.Property
persistentSetGet = Q.monadicIO $ do
    key    <- Q.pick Q.arbitrary
    value  <- Q.pick Q.arbitrary
    store1 <- Q.run newTestStore
    Q.run $ Store.set store1 key (value :: String)
    -- Now Create another store from the same dir to test persistence
    store2 <- Q.run newTestStore
    value' <- Q.run $ Store.get store2 key
    Q.assert $ Store.Found value == value'
    Q.run cleanTestEnv


--------------------------------------------------------------------------------
wrongType :: H.Assertion
wrongType = do
    store <- newTestStore
    -- Store a string and try to fetch an int
    Store.set store ["foo", "bar"] ("qux" :: String)
    value <- Store.get store ["foo", "bar"] :: IO (Store.Result Int)
    H.assert $ case value of
        Store.WrongType e t ->
            e == typeOf (undefined :: Int) &&
            t == typeOf (undefined :: String)
        _                   -> False
