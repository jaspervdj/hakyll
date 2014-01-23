--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Provider.GlobalMetadata.Tests
    ( tests
    ) where

--------------------------------------------------------------------------------
import qualified Data.Map                      as M
import           Control.Monad                 (forM_)
import           Test.Framework                (Test, testGroup)
import           Test.HUnit                    (Assertion, (@=?))


--------------------------------------------------------------------------------
import           Hakyll.Core.Provider          (resourceMetadata)
import           TestSuite.Util

--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Provider.GlobalMetadata.Tests" $
    fromAssertions "page" [testPage]

testPage :: Assertion
testPage = do
    store <- newTestStore
    provider <- newTestProvider store

    metadata <- resourceMetadata provider "posts/2013-10-18-metadata-test.md"
    forM_ ["1", "2", "3", "4", "5", "6", "7", "8"] $ \a ->
        Just a @=? M.lookup ('a':a) metadata

