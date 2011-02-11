-- | This module provides a monadic DSL in which the user can specify the
-- different rules used to run the compilers
--
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Hakyll.Core.Rules
    ( CompileRule (..)
    , RuleSet (..)
    , RulesM
    , Rules
    , runRules
    , compile
    , create
    , route
    , metaCompile
    , metaCompileWith
    ) where

import Control.Applicative (Applicative, (<$>))
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Arrow (second, (>>>), arr, (>>^))
import Control.Monad.State (State, evalState, get, put)
import Data.Monoid (Monoid, mempty, mappend)

import Data.Typeable (Typeable)
import Data.Binary (Binary)

import Hakyll.Core.ResourceProvider
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Routes
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
    { rulesRoutes    :: Routes
    , rulesCompilers :: [(Identifier, Compiler () CompileRule)]
    }

instance Monoid RuleSet where
    mempty = RuleSet mempty mempty
    mappend (RuleSet r1 c1) (RuleSet r2 c2) =
        RuleSet (mappend r1 r2) (mappend c1 c2)

-- | Rule state
--
data RuleState = RuleState
    { rulesMetaCompilerIndex :: Int
    } deriving (Show)

-- | The monad used to compose rules
--
newtype RulesM a = RulesM
    { unRulesM :: ReaderT ResourceProvider (WriterT RuleSet (State RuleState)) a
    } deriving (Monad, Functor, Applicative)

-- | Simplification of the RulesM type; usually, it will not return any
-- result.
--
type Rules = RulesM ()

-- | Run a Rules monad, resulting in a 'RuleSet'
--
runRules :: Rules -> ResourceProvider -> RuleSet
runRules rules provider =
    evalState (execWriterT $ runReaderT (unRulesM rules) provider) state
  where
    state = RuleState {rulesMetaCompilerIndex = 0}

-- | Add a route
--
tellRoute :: Routes -> Rules
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
route :: Pattern -> Routes -> Rules
route pattern route' = tellRoute $ ifMatch pattern route'

-- | Add a compiler that produces other compilers over time
--
metaCompile :: (Binary a, Typeable a, Writable a)
            => Compiler () [(Identifier, Compiler () a)]   
            -- ^ Compiler generating the other compilers
            -> Rules
            -- ^ Resulting rules
metaCompile compiler = RulesM $ do
    -- Create an identifier from the state
    state <- get
    let index = rulesMetaCompilerIndex state
        id' = fromCaptureString "Hakyll.Core.Rules.metaCompile/*" (show index)

    -- Update the state with a new identifier
    put $ state {rulesMetaCompilerIndex = index + 1}

    -- Fallback to 'metaCompileWith' with now known identifier
    unRulesM $ metaCompileWith id' compiler

-- | Version of 'metaCompile' that allows you to specify a custom identifier for
-- the metacompiler.
--
metaCompileWith :: (Binary a, Typeable a, Writable a)
                => Identifier
                -- ^ Identifier for this compiler
                -> Compiler () [(Identifier, Compiler () a)]   
                -- ^ Compiler generating the other compilers
                -> Rules
                -- ^ Resulting rules
metaCompileWith identifier compiler = RulesM $ tell $ RuleSet mempty
    [(identifier, compiler >>> arr makeRule )]
  where
    makeRule = MetaCompileRule . map (second box)
    box = (>>> fromDependency identifier >>^ CompileRule . compiledItem)
