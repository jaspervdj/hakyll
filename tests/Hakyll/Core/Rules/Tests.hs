{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Hakyll.Core.Rules.Tests
    ( tests
    ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Test.Framework
import Test.HUnit hiding (Test)

import Hakyll.Core.Rules
import Hakyll.Core.Rules.Internal
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Routes
import Hakyll.Core.Compiler
import Hakyll.Core.Resource.Provider
import Hakyll.Core.Resource.Provider.Dummy
import Hakyll.Core.Writable.CopyFile
import Hakyll.Web.Page
import TestSuite.Util

tests :: [Test]
tests = fromAssertions "runRules" [case01]

-- | Dummy resource provider
--
provider :: IO ResourceProvider
provider = dummyResourceProvider $ M.fromList $ map (flip (,) "No content")
    [ "posts/a-post.markdown"
    , "posts/some-other-post.markdown"
    ]

-- | Main test
--
case01 :: Assertion
case01 = do
    p <- provider
    let ruleSet = runRules rules p
    expected @=? S.fromList (map fst (rulesCompilers ruleSet))
  where
    expected = S.fromList
        [ Identifier Nothing "posts/a-post.markdown"
        , Identifier Nothing "posts/some-other-post.markdown"
        , Identifier (Just "raw") "posts/a-post.markdown"
        , Identifier (Just "raw") "posts/some-other-post.markdown"
        , Identifier (Just "nav") "posts/a-post.markdown"
        ]

-- | Example rules
--
rules :: Rules
rules = do
    -- Compile some posts
    match "posts/*" $ do
        route $ setExtension "html"
        compile pageCompiler

    -- Compile them, raw
    group "raw" $ do
        route idRoute
        match "posts/*" $ do
            route $ setExtension "html"
            compile getResourceString

    -- Regression test
    group "nav" $ do
        match (list ["posts/a-post.markdown"]) $ do
            route idRoute
            compile copyFileCompiler

    return ()
