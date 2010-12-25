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

import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT)

import Hakyll.Core.Identifier

-- | A lookup with which we can get dependencies
--
type DependencyLookup a = Identifier -> a

-- | Environment for the target monad
--
data TargetEnvironment a = TargetEnvironment
    { targetIdentifier       :: Identifier
    , targetDependencyLookup :: DependencyLookup a  -- ^ Dependency lookup
    }

-- | Monad for targets. In this monad, the user can compose targets and describe
-- how they should be created.
--
newtype TargetM a b = TargetM {unTargetM :: ReaderT (TargetEnvironment a) IO b}
                    deriving (Monad, Functor, MonadIO)

-- | Simplification of the 'TargetM' type for concrete cases: the type of the
-- returned item should equal the type of the dependencies.
--
type Target a = TargetM a a

-- | Run a target, yielding an actual result.
--
runTarget :: Target a -> Identifier -> DependencyLookup a -> IO a
runTarget target id' lookup' = runReaderT (unTargetM target) env
  where
    env = TargetEnvironment
        { targetIdentifier       = id'
        , targetDependencyLookup = lookup'
        }
