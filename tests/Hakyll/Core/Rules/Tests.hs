{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Hakyll.Core.Rules.Tests
    where

import qualified Data.Map as M
import qualified Data.Set as S

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Hakyll.Core.Rules
import Hakyll.Core.Rules.Internal
import Hakyll.Core.Identifier
import Hakyll.Core.Routes
import Hakyll.Core.Compiler
import Hakyll.Core.Resource.Provider
import Hakyll.Core.Resource.Provider.Dummy
import Hakyll.Web.Page

tests :: [Test]
tests =
    [ testCase "runRules" rulesTest
    ]

-- | Main test
--
rulesTest :: Assertion
rulesTest = do
    p <- provider
    let ruleSet = runRules rules p
    assert $ expected == S.fromList (map fst (rulesCompilers ruleSet))
  where
    expected = S.fromList
        [ Identifier Nothing "posts/a-post.markdown"
        , Identifier Nothing "posts/some-other-post.markdown"
        , Identifier (Just "raw") "posts/a-post.markdown"
        , Identifier (Just "raw") "posts/some-other-post.markdown"
        ]

-- | Dummy resource provider
--
provider :: IO ResourceProvider
provider = dummyResourceProvider $ M.fromList $ map (flip (,) "No content")
    [ "posts/a-post.markdown"
    , "posts/some-other-post.markdown"
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

    return ()
