-- | Internal structure of a Target, not exported outside of the library
--
{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}
module Hakyll.Core.Target.Internal
    ( DependencyLookup
    , TargetEnvironment (..)
    , TargetM (..)
    , runTarget
    ) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)

import Hakyll.Core.Identifier
import Hakyll.Core.ResourceProvider
import Hakyll.Core.Store
import Hakyll.Core.CompiledItem

-- | A lookup with which we can get dependencies
--
type DependencyLookup = Identifier -> CompiledItem

-- | Environment for the target monad
--
data TargetEnvironment = TargetEnvironment
    { targetIdentifier       :: Identifier        -- ^ Identifier
    , targetDependencyLookup :: DependencyLookup  -- ^ Dependency lookup
    , targetResourceProvider :: ResourceProvider  -- ^ To get resources
    , targetStore            :: Store             -- ^ Store for caching
    }

-- | State for the target monad
--
data TargetState = TargetState
    { targetSnapshot :: Int  -- ^ Snapshot ID
    }

-- | Monad for targets. In this monad, the user can compose targets and describe
-- how they should be created.
--
newtype TargetM a = TargetM
    { unTargetM :: ReaderT TargetEnvironment (StateT TargetState IO) a
    } deriving (Monad, Functor, Applicative, MonadIO)

-- | Run a target, yielding an actual result.
--
runTarget :: TargetM a
          -> Identifier
          -> DependencyLookup
          -> ResourceProvider
          -> Store
          -> IO a
runTarget target id' lookup' provider store =
    evalStateT (runReaderT (unTargetM target) env) state
  where
    env = TargetEnvironment
        { targetIdentifier       = id'
        , targetDependencyLookup = lookup'
        , targetResourceProvider = provider
        , targetStore            = store
        }
    state = TargetState
        { targetSnapshot = 0
        }
