-- | This module provides a monadic DSL in which the user can specify the
-- different rules used to run the compilers
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Rules
    ( RuleSet (..)
    , RulesM
    , Rules
    , runRules
    , compile
    , create
    , route
    ) where

import Control.Applicative (Applicative, (<$>))
import Control.Monad.Writer
import Control.Monad.Reader

import Hakyll.Core.ResourceProvider
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Compiler
import Hakyll.Core.Route

-- | A collection of rules for the compilation process
--
data RuleSet a = RuleSet
    { rulesRoute     :: Route
    , rulesCompilers :: [(Identifier, Compiler a)]
    }

instance Monoid (RuleSet a) where
    mempty = RuleSet mempty mempty
    mappend (RuleSet r1 c1) (RuleSet r2 c2) =
        RuleSet (mappend r1 r2) (mappend c1 c2)

-- | The monad used to compose rules
--
newtype RulesM a b = RulesM
    { unRulesM :: ReaderT ResourceProvider (Writer (RuleSet a)) b
    } deriving (Monad, Functor, Applicative)

-- | Simplification of the RulesM type; usually, it will not return any
-- result.
--
type Rules a = RulesM a ()

-- | Run a Rules monad, resulting in a 'RuleSet'
--
runRules :: Rules a -> ResourceProvider -> RuleSet a
runRules rules provider = execWriter $ runReaderT (unRulesM rules) provider

-- | Add a route
--
addRoute :: Route -> Rules a
addRoute route' = RulesM $ tell $ RuleSet route' mempty

-- | Add a number of compilers
--
addCompilers :: [(Identifier, Compiler a)] -> Rules a
addCompilers compilers = RulesM $ tell $ RuleSet mempty compilers

-- | Add a compilation rule
--
-- This instructs all resources matching the given pattern to be compiled using
-- the given compiler
--
compile :: Pattern -> Compiler a -> Rules a
compile pattern compiler = RulesM $ do
    identifiers <- matches pattern . resourceList <$> ask
    unRulesM $ addCompilers $ zip identifiers (repeat compiler)

-- | Add a compilation rule
--
-- This sets a compiler for the given identifier
--
create :: Identifier -> Compiler a -> RulesM a ()
create identifier compiler = addCompilers [(identifier, compiler)]

-- | Add a route
--
route :: Pattern -> Route -> RulesM a ()
route pattern route' = addRoute $ ifMatch pattern route'
