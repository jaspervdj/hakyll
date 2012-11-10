--------------------------------------------------------------------------------
-- | Internally used compiler module
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


--------------------------------------------------------------------------------
import           Control.Applicative          (Alternative (..), Applicative,
                                               pure, (<$>), (<*>))
import           Control.Arrow
import           Control.Category             (Category, id, (.))
import           Control.Monad                (liftM2, (<=<))
import           Control.Monad.Error          (ErrorT, catchError, runErrorT,
                                               throwError)
import           Control.Monad.Reader         (Reader, ReaderT, ask, runReader,
                                               runReaderT)
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Prelude                      hiding (id, (.))


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Logger
import           Hakyll.Core.ResourceProvider
import           Hakyll.Core.Routes
import           Hakyll.Core.Store
import           Hakyll.Core.Util.Arrow


--------------------------------------------------------------------------------
-- | A set of dependencies
type Dependencies = Set (Identifier ())


--------------------------------------------------------------------------------
-- | Environment in which the dependency analyzer runs
data DependencyEnvironment = DependencyEnvironment
    { -- | Target identifier
      dependencyIdentifier :: Identifier ()
    , -- | List of available identifiers we can depend upon
      dependencyUniverse   :: [Identifier ()]
    }


--------------------------------------------------------------------------------
-- | Environment in which a compiler runs
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


--------------------------------------------------------------------------------
-- | A calculation possibly throwing an error
type Throwing a = Either String a


--------------------------------------------------------------------------------
-- | The compiler monad
newtype CompilerM a = CompilerM
    { unCompilerM :: ErrorT String (ReaderT CompilerEnvironment IO) a
    } deriving (Monad, Functor, Applicative)


--------------------------------------------------------------------------------
-- | The compiler arrow
data Compiler a b = Compiler
    { compilerDependencies :: Reader DependencyEnvironment Dependencies
    , compilerJob          :: a -> CompilerM b
    }


--------------------------------------------------------------------------------
instance Functor (Compiler a) where
    fmap f ~(Compiler d j) = Compiler d $ fmap f . j


--------------------------------------------------------------------------------
instance Applicative (Compiler a) where
    pure = fromJob . const . return
    ~(Compiler d1 j1) <*> ~(Compiler d2 j2) =
        Compiler (liftM2 S.union d1 d2) $ \x -> j1 x <*> j2 x


--------------------------------------------------------------------------------
instance Alternative (Compiler a) where
    empty = fromJob $ const $ CompilerM $
        throwError "Hakyll.Core.Compiler.Internal: empty alternative"
    ~(Compiler d1 j1) <|> ~(Compiler d2 j2) =
        Compiler (liftM2 S.union d1 d2) $ \x -> CompilerM $
            catchError (unCompilerM $ j1 x) (\_ -> unCompilerM $ j2 x)


--------------------------------------------------------------------------------
instance Category Compiler where
    id = Compiler (return S.empty) return
    ~(Compiler d1 j1) . ~(Compiler d2 j2) =
        Compiler (liftM2 S.union d1 d2) (j1 <=< j2)


--------------------------------------------------------------------------------
instance Arrow Compiler where
    arr f = fromJob (return . f)
    first ~(Compiler d j) = Compiler d $ \(x, y) -> do
        x' <- j x
        return (x', y)


--------------------------------------------------------------------------------
instance ArrowChoice Compiler where
    left ~(Compiler d j) = Compiler d $ \e -> case e of
        Left l  -> Left  <$> j l
        Right r -> Right <$> return r
    -- Defined here for efficiency
    ~(Compiler d1 j1) ||| ~(Compiler d2 j2) = Compiler (liftM2 S.union d1 d2) $
        \e -> case e of Left x  -> j1 x; Right y -> j2 y


--------------------------------------------------------------------------------
instance ArrowMap Compiler where
    mapA (Compiler d j) = Compiler d $ mapM j


--------------------------------------------------------------------------------
-- | Run a compiler, yielding the resulting target
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


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
fromJob :: (a -> CompilerM b) -> Compiler a b
fromJob = Compiler $ return S.empty
{-# INLINE fromJob #-}


--------------------------------------------------------------------------------
fromDependencies :: (Identifier () -> [Identifier ()] -> [Identifier ()])
                 -> Compiler b b
fromDependencies collectDeps = flip Compiler return $ do
    DependencyEnvironment identifier universe <- ask
    return $ S.fromList $ collectDeps identifier universe


--------------------------------------------------------------------------------
-- | Wait until another compiler has finished before running this compiler
fromDependency :: Identifier a -> Compiler b b
fromDependency = fromDependencies . const . const . return . castIdentifier
