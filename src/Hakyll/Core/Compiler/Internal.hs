-- | Internally used compiler module
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Compiler.Internal
    ( Dependencies
    , DependencyEnvironment (..)
    , CompilerEnvironment (..)
    , Throwing
    , CompilerM (..)
    , Compiler (..)
    , runCompilerJob
    , runCompilerDependencies
    , fromJob
    , fromDependencies
    , fromDependency
    ) where

import Prelude hiding ((.), id)
import Control.Applicative (Applicative, pure, (<*>), (<$>))
import Control.Monad.Reader (ReaderT, Reader, ask, runReaderT, runReader)
import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad ((<=<), liftM2)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Category (Category, (.), id)
import Control.Arrow (Arrow, ArrowChoice, arr, first, left)

import Hakyll.Core.Identifier
import Hakyll.Core.ResourceProvider
import Hakyll.Core.Store
import Hakyll.Core.Routes
import Hakyll.Core.Logger

-- | A set of dependencies
--
type Dependencies = Set (Identifier ())

-- | Environment in which the dependency analyzer runs
--
data DependencyEnvironment = DependencyEnvironment
    { -- | Target identifier
      dependencyIdentifier :: Identifier ()
    , -- | List of available identifiers we can depend upon
      dependencyUniverse   :: [Identifier ()]
    }

-- | Environment in which a compiler runs
--
data CompilerEnvironment = CompilerEnvironment
    { -- | Target identifier
      compilerIdentifier       :: Identifier ()
    , -- | Resource provider
      compilerResourceProvider :: ResourceProvider
    , -- | List of all known identifiers
      compilerUniverse         :: [Identifier ()]
    , -- | Site routes
      compilerRoutes           :: Routes
    , -- | Compiler store
      compilerStore            :: Store
    , -- | Flag indicating if the underlying resource was modified
      compilerResourceModified :: Bool
    , -- | Logger
      compilerLogger           :: Logger
    }

-- | A calculation possibly throwing an error
--
type Throwing a = Either String a

-- | The compiler monad
--
newtype CompilerM a = CompilerM
    { unCompilerM :: ErrorT String (ReaderT CompilerEnvironment IO) a
    } deriving (Monad, Functor, Applicative)

-- | The compiler arrow
--
data Compiler a b = Compiler
    { compilerDependencies :: Reader DependencyEnvironment Dependencies
    , compilerJob          :: a -> CompilerM b
    }

instance Functor (Compiler a) where
    fmap f ~(Compiler d j) = Compiler d $ fmap f . j

instance Applicative (Compiler a) where
    pure = Compiler (return S.empty) . const . return
    ~(Compiler d1 f) <*> ~(Compiler d2 j) =
        Compiler (liftM2 S.union d1 d2) $ \x -> f x <*> j x

instance Category Compiler where
    id = Compiler (return S.empty) return
    ~(Compiler d1 j1) . ~(Compiler d2 j2) =
        Compiler (liftM2 S.union d1 d2) (j1 <=< j2)

instance Arrow Compiler where
    arr f = Compiler (return S.empty) (return . f)
    first ~(Compiler d j) = Compiler d $ \(x, y) -> do
        x' <- j x
        return (x', y)

instance ArrowChoice Compiler where
    left ~(Compiler d j) = Compiler d $ \e -> case e of
        Left l  -> Left  <$> j l
        Right r -> Right <$> return r

-- | Run a compiler, yielding the resulting target
--
runCompilerJob :: Compiler () a     -- ^ Compiler to run
               -> Identifier ()     -- ^ Target identifier
               -> ResourceProvider  -- ^ Resource provider
               -> [Identifier ()]   -- ^ Universe
               -> Routes            -- ^ Route
               -> Store             -- ^ Store
               -> Bool              -- ^ Was the resource modified?
               -> Logger            -- ^ Logger
               -> IO (Throwing a)   -- ^ Result
runCompilerJob compiler id' provider universe route store modified logger =
    runReaderT (runErrorT $ unCompilerM $ compilerJob compiler ()) env
  where
    env = CompilerEnvironment
            { compilerIdentifier       = id'
            , compilerResourceProvider = provider
            , compilerUniverse         = universe
            , compilerRoutes           = route
            , compilerStore            = store
            , compilerResourceModified = modified
            , compilerLogger           = logger
            }

runCompilerDependencies :: Compiler () a
                        -> Identifier ()
                        -> [Identifier ()]
                        -> Dependencies
runCompilerDependencies compiler identifier universe =
    runReader (compilerDependencies compiler) env
  where
    env = DependencyEnvironment
            { dependencyIdentifier = identifier
            , dependencyUniverse   = universe
            }

fromJob :: (a -> CompilerM b)
        -> Compiler a b
fromJob = Compiler (return S.empty)

fromDependencies :: (Identifier () -> [Identifier ()] -> [Identifier ()])
                 -> Compiler b b
fromDependencies collectDeps = flip Compiler return $ do
    DependencyEnvironment identifier universe <- ask
    return $ S.fromList $ collectDeps identifier universe

-- | Wait until another compiler has finished before running this compiler
--
fromDependency :: Identifier a -> Compiler b b
fromDependency = fromDependencies . const . const . return . castIdentifier
