{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Hakyll.Core.Rules.Tests
    where

import qualified Data.Map as M

import Test.Framework

import Hakyll.Core.Rules
import Hakyll.Core.Identifier
import Hakyll.Core.Routes
import Hakyll.Core.Compiler
import Hakyll.Core.Resource.Provider
import Hakyll.Core.Resource.Provider.Dummy
import Hakyll.Web.Page

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
        match "posts/*" $ do
            route $ setExtension "html"
            compile getResourceString

    return ()
