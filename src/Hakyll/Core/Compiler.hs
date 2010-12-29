-- | A Compiler manages targets and dependencies between targets.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Compiler
    ( Dependencies
    , CompilerM
    , Compiler
    , runCompiler
    , require
    , requireAll
    , compileFromString
    ) where

import Control.Arrow (second)
import Control.Applicative (Applicative, (<$>))
import Control.Monad.State (State, modify, runState)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Typeable (Typeable)
import Data.Binary (Binary)

import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Target
import Hakyll.Core.Target.Internal
import Hakyll.Core.CompiledItem
import Hakyll.Core.Writable
import Hakyll.Core.ResourceProvider

-- | A set of dependencies
--
type Dependencies = Set Identifier

-- | Add one dependency
--
addDependency :: Identifier -> CompilerM ()
addDependency dependency = CompilerM $ modify $ addDependency'
  where
    addDependency' x = x
        { compilerDependencies = S.insert dependency $ compilerDependencies x
        }

-- | Environment in which a compiler runs
--
data CompilerEnvironment = CompilerEnvironment
    { compilerIdentifier       :: Identifier        -- ^ Target identifier
    , compilerResourceProvider :: ResourceProvider  -- ^ Resource provider
    }

-- | State carried along by a compiler
--
data CompilerState = CompilerState
    { compilerDependencies :: Dependencies
    }

-- | The compiler monad
--
newtype CompilerM a = CompilerM
    { unCompilerM :: ReaderT CompilerEnvironment (State CompilerState) a
    } deriving (Monad, Functor, Applicative)

-- | Simplified type for a compiler generating a target (which covers most
-- cases)
--
type Compiler a = CompilerM (TargetM a)

-- | Run a compiler, yielding the resulting target and it's dependencies
--
runCompiler :: Compiler a -> Identifier -> ResourceProvider
            -> (TargetM a, Dependencies)
runCompiler compiler identifier provider = second compilerDependencies $
    runState (runReaderT (unCompilerM compiler) env) state
  where
    state = CompilerState S.empty
    env = CompilerEnvironment
            { compilerIdentifier       = identifier
            , compilerResourceProvider = provider
            }


-- | Require another target. Using this function ensures automatic handling of
-- dependencies
--
require :: (Binary a, Typeable a, Writable a)
        => Identifier
        -> Compiler a
require identifier = do
    addDependency identifier
    return $ TargetM $ do
        lookup' <- targetDependencyLookup <$> ask
        return $ unCompiledItem $ lookup' identifier

-- | Require a number of targets. Using this function ensures automatic handling
-- of dependencies
--
requireAll :: (Binary a, Typeable a, Writable a)
           => Pattern
           -> Compiler [a]
requireAll pattern = CompilerM $ do
    provider <- compilerResourceProvider <$> ask
    r <- unCompilerM $ mapM require $ matches pattern $ resourceList provider
    return $ sequence r

-- | Construct a target from a string, this string being the content of the
-- resource.
--
compileFromString :: (String -> TargetM a)  -- ^ Function to create the target
                  -> Compiler a             -- ^ Resulting compiler
compileFromString = return . (getResourceString >>=)
