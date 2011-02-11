-- | Internal rules module for types which are not exposed to the user
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Rules.Internal
    ( CompileRule (..)
    , RuleSet (..)
    , RuleState (..)
    , RulesM (..)
    , Rules
    , runRules
    ) where

import Control.Applicative (Applicative)
import Control.Monad.Writer (WriterT, execWriterT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (State, evalState)
import Data.Monoid (Monoid, mempty, mappend)

import Hakyll.Core.ResourceProvider
import Hakyll.Core.Identifier
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Routes
import Hakyll.Core.CompiledItem

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
