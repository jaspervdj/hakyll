-- | This module provides a monadic DSL in which the user can specify the
-- different rules used to run the compilers
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Rules
    ( CompileRule (..)
    , RuleSet (..)
    , RulesM
    , Rules
    , runRules
    , compile
    , create
    , route
    , addCompilers
    ) where

import Control.Applicative (Applicative, (<$>))
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Arrow (second, (>>>), arr, (>>^))

import Data.Typeable (Typeable)
import Data.Binary (Binary)

import Hakyll.Core.ResourceProvider
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Route
import Hakyll.Core.CompiledItem
import Hakyll.Core.Writable

-- | Output of a compiler rule
--
-- * The compiler will produce a simple item. This is the most common case.
--
-- * The compiler will produce more compilers. These new compilers need to be
--   added to the runtime if possible, since other items might depend upon them.
--
data CompileRule = CompileRule CompiledItem
                 | MetaCompileRule [(Identifier, Compiler () CompileRule)]

-- | A collection of rules for the compilation process
--
data RuleSet = RuleSet
    { rulesRoute     :: Route
    , rulesCompilers :: [(Identifier, Compiler () CompileRule)]
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
tellRoute :: Route -> Rules
tellRoute route' = RulesM $ tell $ RuleSet route' mempty

-- | Add a number of compilers
--
tellCompilers :: (Binary a, Typeable a, Writable a)
             => [(Identifier, Compiler () a)]
             -> Rules
tellCompilers compilers = RulesM $ tell $ RuleSet mempty $
    map (second boxCompiler) compilers
  where
    boxCompiler = (>>> arr compiledItem >>> arr CompileRule)

-- | Add a compilation rule
--
-- This instructs all resources matching the given pattern to be compiled using
-- the given compiler
--
compile :: (Binary a, Typeable a, Writable a)
        => Pattern -> Compiler () a -> Rules
compile pattern compiler = RulesM $ do
    identifiers <- matches pattern . resourceList <$> ask
    unRulesM $ tellCompilers $ zip identifiers (repeat compiler)

-- | Add a compilation rule
--
-- This sets a compiler for the given identifier
--
create :: (Binary a, Typeable a, Writable a)
       => Identifier -> Compiler () a -> Rules
create identifier compiler = tellCompilers [(identifier, compiler)]

-- | Add a route
--
route :: Pattern -> Route -> Rules
route pattern route' = tellRoute $ ifMatch pattern route'

-- | Add a compiler that produces other compilers over time
--
addCompilers :: (Binary a, Typeable a, Writable a)
             => Identifier
             -- ^ Identifier for this compiler
             -> Compiler () [(Identifier, Compiler () a)]   
             -- ^ Compiler generating the other compilers
             -> Rules
             -- ^ Resulting rules
addCompilers identifier compiler = RulesM $ tell $ RuleSet mempty $
    [(identifier, compiler >>> arr makeRule )]
  where
    makeRule = MetaCompileRule . map (second box)
    box = (>>> fromDependency identifier >>^ CompileRule . compiledItem)
