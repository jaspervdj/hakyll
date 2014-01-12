--------------------------------------------------------------------------------
-- | This module provides a declarative DSL in which the user can specify the
-- different rules used to run the compilers.
--
-- The convention is to just list all items in the 'Rules' monad, routes and
-- compilation rules.
--
-- A typical usage example would be:
--
-- > main = hakyll $ do
-- >     match "posts/*" $ do
-- >         route   (setExtension "html")
-- >         compile someCompiler
-- >     match "css/*" $ do
-- >         route   idRoute
-- >         compile compressCssCompiler
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Hakyll.Core.Rules
    ( Rules
    , match
    , create
    , version
    , compile
    , route

      -- * Advanced usage
    , preprocess
    , Dependency (..)
    , rulesExtraDependencies
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            ((<$>))
import           Control.Monad.Reader           (ask, local)
import           Control.Monad.Trans            (liftIO)
import           Control.Monad.Writer           (tell)
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    (mempty)


--------------------------------------------------------------------------------
import           Data.Binary                    (Binary)
import           Data.Typeable                  (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Item
import           Hakyll.Core.Item.SomeItem
import           Hakyll.Core.Routes
import           Hakyll.Core.Rules.Internal
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
tellRulesItem :: RulesItem -> Rules ()
tellRulesItem ri = Rules $ do
    pattern      <- fromMaybe mempty . rulesPattern <$> ask
    create'      <- rulesCreate                     <$> ask
    version'     <- rulesVersion                    <$> ask
    dependencies <- rulesDependencies               <$> ask

    let ri' = addDependencies dependencies ri
    case create' of
        [] -> tell mempty {rulesMatched = [(pattern, version', ri')]}
        ls -> tell mempty {rulesCreated = [(i, version', ri') | i <- ls]}
  where
    addDependencies [] x                 = x
    addDependencies ds (RulesItem mr mc) =
        RulesItem mr (fmap (compilerTellDependencies ds >>) mc)


--------------------------------------------------------------------------------
match :: Pattern -> Rules () -> Rules ()
match pattern =
    Rules . local (\r -> r {rulesPattern = Just pattern}) . unRules


--------------------------------------------------------------------------------
create :: [Identifier] -> Rules () -> Rules ()
create ids =
    Rules . local (\r -> r {rulesCreate = ids}) . unRules


--------------------------------------------------------------------------------
version :: String -> Rules () -> Rules ()
version v =
    Rules . local (\r -> r {rulesVersion = Just v}) . unRules


--------------------------------------------------------------------------------
-- | Add a compilation rule to the rules.
--
-- This instructs all resources to be compiled using the given compiler.
compile :: (Binary a, Typeable a, Writable a) => Compiler (Item a) -> Rules ()
compile compiler = tellRulesItem $ RulesItem Nothing (Just compiler')
  where
    compiler' = fmap SomeItem compiler


--------------------------------------------------------------------------------
-- | Add a route.
--
-- This adds a route for all items matching the current pattern.
route :: Routes -> Rules ()
route route' = tellRulesItem $ RulesItem (Just route') Nothing


--------------------------------------------------------------------------------
-- | Execute an 'IO' action immediately while the rules are being evaluated.
-- This should be avoided if possible, but occasionally comes in useful.
preprocess :: IO a -> Rules a
preprocess = Rules . liftIO


--------------------------------------------------------------------------------
-- | Advanced usage: add extra dependencies to compilers. Basically this is
-- needed when you're doing unsafe tricky stuff in the rules monad, but you
-- still want correct builds.
--
-- A useful utility for this purpose is 'makePatternDependency'.
rulesExtraDependencies :: [Dependency] -> Rules a -> Rules a
rulesExtraDependencies deps =
    Rules . local (\r -> r {rulesDependencies = deps}) . unRules
