-- | A Compiler manages targets and dependencies between targets.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Compiler
    ( Dependencies
    , CompilerM
    , Compiler (..)
    , runCompiler
    , getIdentifier
    , getResourceString
    , require
    -- , requireAll
    -- , compileFromString
    ) where

import Prelude hiding ((.), id)
import Control.Arrow (second, (>>>))
import Control.Applicative (Applicative, (<$>))
import Control.Monad.State (State, modify, runState)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (liftIO)
import Control.Monad ((<=<))
import Data.Set (Set)
import qualified Data.Set as S
import Control.Category (Category, (.), id)
import Control.Arrow (Arrow, arr, first)

import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.CompiledItem
import Hakyll.Core.Writable
import Hakyll.Core.ResourceProvider

-- | A set of dependencies
--
type Dependencies = Set Identifier

-- | A lookup with which we can get dependencies
--
type DependencyLookup = Identifier -> CompiledItem

-- | Environment in which a compiler runs
--
data CompilerEnvironment = CompilerEnvironment
    { compilerIdentifier       :: Identifier        -- ^ Target identifier
    , compilerResourceProvider :: ResourceProvider  -- ^ Resource provider
    , compilerDependencyLookup :: DependencyLookup  -- ^ Dependency lookup
    }

-- | The compiler monad
--
newtype CompilerM a = CompilerM
    { unCompilerM :: ReaderT CompilerEnvironment IO a
    } deriving (Monad, Functor, Applicative)

-- | The compiler arrow
--
data Compiler a b = Compiler
    { -- TODO: Reader ResourceProvider Dependencies
      compilerDependencies :: Dependencies
    , compilerJob          :: a -> CompilerM b
    }

instance Category Compiler where
    id = Compiler S.empty return
    (Compiler d1 j1) . (Compiler d2 j2) =
        Compiler (d1 `S.union` d2) (j1 <=< j2)

instance Arrow Compiler where
    arr f = Compiler S.empty (return . f)
    first (Compiler d j) = Compiler d $ \(x, y) -> do
        x' <- j x
        return (x', y)

-- | Run a compiler, yielding the resulting target and it's dependencies
--
runCompiler :: Compiler () a
            -> Identifier
            -> ResourceProvider
            -> DependencyLookup
            -> IO a
runCompiler compiler identifier provider lookup' =
    runReaderT (unCompilerM $ compilerJob compiler ()) env
  where
    env = CompilerEnvironment
            { compilerIdentifier       = identifier
            , compilerResourceProvider = provider
            , compilerDependencyLookup = lookup'
            }

addDependency :: Identifier
              -> Compiler b b
addDependency id' = Compiler (S.singleton id') return

fromCompilerM :: (a -> CompilerM b)
              -> Compiler a b
fromCompilerM = Compiler S.empty

getIdentifier :: Compiler () Identifier
getIdentifier = fromCompilerM $ const $ CompilerM $
    compilerIdentifier <$> ask

getResourceString :: Compiler () String
getResourceString = getIdentifier >>> getResourceString'
  where
    getResourceString' = fromCompilerM $ \id' -> CompilerM $ do
        provider <- compilerResourceProvider <$> ask
        liftIO $ resourceString provider id'

-- | Require another target. Using this function ensures automatic handling of
-- dependencies
--
require :: (Binary a, Typeable a, Writable a)
        => Identifier
        -> (a -> b -> c)
        -> Compiler b c
require identifier f = addDependency identifier >>> fromCompilerM require'
  where
    require' x = CompilerM $ do
        lookup' <- compilerDependencyLookup <$> ask
        return $ f (unCompiledItem $ lookup' identifier) x

{-
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
-}
