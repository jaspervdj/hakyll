{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Identifier.Tests
    ( tests
    ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Hakyll.Core.Identifier.Pattern

tests :: [Test]
tests = zipWith testCase names matchCases 
  where
    names = map (\n -> "match [" ++ show n ++ "]") [1 :: Int ..]

-- | Collection of simple cases
--
matchCases :: [Assertion]
matchCases =
    [ Just [["bar"]]                 @=? match "foo/**" "foo/bar"
    , Just [["foo", "bar"]]          @=? match "**" "foo/bar"
    , Nothing                        @=? match "*" "foo/bar"
    , Just []                        @=? match "foo" "foo"
    , Just [["foo"]]                 @=? match "*/bar" "foo/bar"
    , Just [["foo", "bar"]]          @=? match "**/qux" "foo/bar/qux"
    , Just [["foo", "bar"], ["qux"]] @=? match "**/*" "foo/bar/qux"
    , Just [["foo"], ["bar", "qux"]] @=? match "*/**" "foo/bar/qux"
    ]
