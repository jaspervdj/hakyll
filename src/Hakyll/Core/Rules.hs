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
import Control.Arrow (second)

import Data.Typeable (Typeable)
import Data.Binary (Binary)

import Hakyll.Core.ResourceProvider
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Compiler
import Hakyll.Core.Route
import Hakyll.Core.CompiledItem
import Hakyll.Core.Writable

-- | A collection of rules for the compilation process
--
data RuleSet = RuleSet
    { rulesRoute     :: Route
    , rulesCompilers :: [(Identifier, Compiler CompiledItem)]
    }

instance Monoid RuleSet where
    mempty = RuleSet mempty mempty
    mappend (RuleSet r1 c1) (RuleSet r2 c2) =
        RuleSet (mappend r1 r2) (mappend c1 c2)

-- | The monad used to compose rules
--
newtype RulesM a = RulesM
    { unRulesM :: ReaderT ResourceProvider (Writer RuleSet) a
    } deriving (Monad, Functor, Applicative)

-- | Simplification of the RulesM type; usually, it will not return any
-- result.
--
type Rules = RulesM ()

-- | Run a Rules monad, resulting in a 'RuleSet'
--
runRules :: Rules -> ResourceProvider -> RuleSet
runRules rules provider = execWriter $ runReaderT (unRulesM rules) provider

-- | Add a route
--
addRoute :: Route -> Rules
addRoute route' = RulesM $ tell $ RuleSet route' mempty

-- | Add a number of compilers
--
addCompilers :: (Binary a, Typeable a, Writable a)
             => [(Identifier, Compiler a)]
             -> Rules
addCompilers compilers = RulesM $ tell $ RuleSet mempty $
    map (second boxCompiler) compilers
  where
    boxCompiler = fmap (fmap compiledItem)

-- | Add a compilation rule
--
-- This instructs all resources matching the given pattern to be compiled using
-- the given compiler
--
compile :: (Binary a, Typeable a, Writable a)
        => Pattern -> Compiler a -> Rules
compile pattern compiler = RulesM $ do
    identifiers <- matches pattern . resourceList <$> ask
    unRulesM $ addCompilers $ zip identifiers (repeat compiler)

-- | Add a compilation rule
--
-- This sets a compiler for the given identifier
--
create :: (Binary a, Typeable a, Writable a)
       => Identifier -> Compiler a -> Rules
create identifier compiler = addCompilers [(identifier, compiler)]

-- | Add a route
--
route :: Pattern -> Route -> Rules
route pattern route' = addRoute $ ifMatch pattern route'
