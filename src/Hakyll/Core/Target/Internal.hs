-- | Internal structure of a Target, not exported outside of the library
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Target.Internal
    ( DependencyLookup
    , TargetEnvironment (..)
    , TargetM (..)
    , Target
    , runTarget
    ) where

import Control.Applicative (Applicative)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)

import Hakyll.Core.Identifier
import Hakyll.Core.ResourceProvider
import Hakyll.Core.Store

-- | A lookup with which we can get dependencies
--
type DependencyLookup a = Identifier -> a

-- | Environment for the target monad
--
data TargetEnvironment a = TargetEnvironment
    { targetIdentifier       :: Identifier          -- ^ Identifier
    , targetDependencyLookup :: DependencyLookup a  -- ^ Dependency lookup
    , targetResourceProvider :: ResourceProvider    -- ^ To get resources
    , targetStore            :: Store               -- ^ Store for caching
    }

-- | State for the target monad
--
data TargetState = TargetState
    { targetSnapshot :: Int  -- ^ Snapshot ID
    }

-- | Monad for targets. In this monad, the user can compose targets and describe
-- how they should be created.
--
newtype TargetM a b = TargetM
    { unTargetM :: ReaderT (TargetEnvironment a) (StateT TargetState IO) b
    } deriving (Monad, Functor, Applicative, MonadIO)

-- | Simplification of the 'TargetM' type for concrete cases: the type of the
-- returned item should equal the type of the dependencies.
--
type Target a = TargetM a a

-- | Run a target, yielding an actual result.
--
runTarget :: Target a
          -> Identifier
          -> DependencyLookup a
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
