--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Store.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.Typeable           (typeOf)
import qualified Test.QuickCheck         as Q
import qualified Test.QuickCheck.Monadic as Q
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase)
import qualified Test.Tasty.HUnit        as H
import           Test.Tasty.QuickCheck   (testProperty)


--------------------------------------------------------------------------------
import qualified Hakyll.Core.Store       as Store
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Core.Store.Tests"
    [ testProperty "simple get . set"     simpleSetGet
    , testProperty "persistent get . set" persistentSetGet
    , testCase     "WrongType get . set"  wrongType
    , testCase     "isMembertest . set"   isMembertest
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
    case value of
        Store.WrongType e t -> do
            typeOf (undefined :: Int)    H.@=? e
            typeOf (undefined :: String) H.@=? t
        _ -> H.assertFailure "Expecting WrongType"
    cleanTestEnv


--------------------------------------------------------------------------------
isMembertest :: H.Assertion
isMembertest = do
    store <- newTestStore
    Store.set store ["foo", "bar"] ("qux" :: String)
    good <- Store.isMember store ["foo", "bar"]
    bad  <- Store.isMember store ["foo", "baz"]
    True  H.@=? good
    False H.@=? bad
    cleanTestEnv
