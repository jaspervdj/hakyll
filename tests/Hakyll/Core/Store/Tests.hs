{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Store.Tests
    ( tests
    ) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Test.HUnit as H

import Hakyll.Core.Identifier
import Hakyll.Core.Store

tests :: [Test]
tests =
    [ testProperty "simple storeGet . storeSet" simpleSetGet
    , testProperty "persistent storeGet . storeSet" persistentSetGet
    , testCase     "WrongType storeGet . storeSet" wrongType
    ]

simpleSetGet :: Property
simpleSetGet = monadicIO $ do
    identifier <- parseIdentifier . unFileName <$> pick arbitrary
    FileName name <- pick arbitrary
    value <- pick arbitrary
    store <- run $ makeStore "_store"
    run $ storeSet store name identifier (value :: String)
    value' <- run $ storeGet store name identifier
    assert $ Found value == value'

persistentSetGet :: Property
persistentSetGet = monadicIO $ do
    identifier <- parseIdentifier . unFileName <$> pick arbitrary
    FileName name <- pick arbitrary
    value <- pick arbitrary
    store1 <- run $ makeStore "_store"
    run $ storeSet store1 name identifier (value :: String)
    -- Now Create another store from the same dir to test persistence
    store2 <- run $ makeStore "_store"
    value' <- run $ storeGet store2 name identifier
    assert $ Found value == value'

wrongType :: H.Assertion
wrongType = do
    store <- makeStore "_store"
    -- Store a string and try to fetch an int
    storeSet store "foo" "bar" ("qux" :: String)
    value <- storeGet store "foo" "bar" :: IO (StoreGet Int)
    H.assert $ case value of WrongType _ _ -> True
                             _             -> False

newtype FileName = FileName {unFileName :: String}
                 deriving (Show)

instance Arbitrary FileName where
    arbitrary = do
        length' <- choose (5, 100)
        str <- replicateM length' $ elements cs
        return $ FileName str
      where
        cs = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ".- "
