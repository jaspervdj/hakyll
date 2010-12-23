{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Route.Tests
    ( tests
    ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Hakyll.Core.Route

tests :: [Test]
tests = zipWith testCase names matchCases 
  where
    names = map (\n -> "runRoute [" ++ show n ++ "]") [1 :: Int ..]

-- | Collection of simple cases
--
matchCases :: [Assertion]
matchCases =
    [ Just "foo.html" @=? runRoute (setExtension "html") "foo"
    , Just "foo.html" @=? runRoute (setExtension ".html") "foo"
    , Just "foo.html" @=? runRoute (setExtension "html") "foo.markdown"
    , Just "foo.html" @=? runRoute (setExtension ".html") "foo.markdown"
    ]
