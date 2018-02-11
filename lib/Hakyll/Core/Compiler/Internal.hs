--------------------------------------------------------------------------------
-- | Internally used compiler module
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Hakyll.Core.Compiler.Internal
    ( -- * Types
      Snapshot
    , CompilerRead (..)
    , CompilerWrite (..)
    , CompilerResult (..)
    , Compiler (..)
    , runCompiler

      -- * Core operations
    , compilerTell
    , compilerAsk
    , compilerThrow
    , compilerCatch
    , compilerResult
    , compilerUnsafeIO

      -- * Utilities
    , compilerTellDependencies
    , compilerTellCacheHits
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            (Alternative (..))
import           Control.Exception              (SomeException, handle)
import           Control.Monad                  (forM_)
import           Control.Monad.Except            (MonadError (..))
#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup                 (Semigroup (..))
#endif
import           Data.Set                       (Set)
import qualified Data.Set                       as S


--------------------------------------------------------------------------------
import           Hakyll.Core.Configuration
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Logger             (Logger)
import qualified Hakyll.Core.Logger             as Logger
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider
import           Hakyll.Core.Routes
import           Hakyll.Core.Store


--------------------------------------------------------------------------------
-- | Whilst compiling an item, it possible to save multiple snapshots of it, and
-- not just the final result.
type Snapshot = String


--------------------------------------------------------------------------------
-- | Environment in which a compiler runs
data CompilerRead = CompilerRead
    { -- | Main configuration
      compilerConfig     :: Configuration
    , -- | Underlying identifier
      compilerUnderlying :: Identifier
    , -- | Resource provider
      compilerProvider   :: Provider
    , -- | List of all known identifiers
      compilerUniverse   :: Set Identifier
    , -- | Site routes
      compilerRoutes     :: Routes
    , -- | Compiler store
      compilerStore      :: Store
    , -- | Logger
      compilerLogger     :: Logger
    }


--------------------------------------------------------------------------------
data CompilerWrite = CompilerWrite
    { compilerDependencies :: [Dependency]
    , compilerCacheHits    :: Int
    } deriving (Show)


--------------------------------------------------------------------------------
#if MIN_VERSION_base(4,9,0)
instance Semigroup CompilerWrite where
    (<>) (CompilerWrite d1 h1) (CompilerWrite d2 h2) =
        CompilerWrite (d1 ++ d2) (h1 + h2)

instance Monoid CompilerWrite where
    mempty  = CompilerWrite [] 0
    mappend = (<>)
#else
instance Monoid CompilerWrite where
    mempty = CompilerWrite [] 0
    mappend (CompilerWrite d1 h1) (CompilerWrite d2 h2) =
        CompilerWrite (d1 ++ d2) (h1 + h2)
#endif


--------------------------------------------------------------------------------
data CompilerResult a where
    CompilerDone     :: a -> CompilerWrite -> CompilerResult a
    CompilerSnapshot :: Snapshot -> Compiler a -> CompilerResult a
    CompilerError    :: [String] -> CompilerResult a
    CompilerRequire  :: (Identifier, Snapshot) -> Compiler a -> CompilerResult a


--------------------------------------------------------------------------------
-- | A monad which lets you compile items and takes care of dependency tracking
-- for you.
newtype Compiler a = Compiler
    { unCompiler :: CompilerRead -> IO (CompilerResult a)
    }


--------------------------------------------------------------------------------
instance Functor Compiler where
    fmap f (Compiler c) = Compiler $ \r -> do
        res <- c r
        return $ case res of
            CompilerDone x w      -> CompilerDone (f x) w
            CompilerSnapshot s c' -> CompilerSnapshot s (fmap f c')
            CompilerError e       -> CompilerError e
            CompilerRequire i c'  -> CompilerRequire i (fmap f c')
    {-# INLINE fmap #-}


--------------------------------------------------------------------------------
instance Monad Compiler where
    return x = Compiler $ \_ -> return $ CompilerDone x mempty
    {-# INLINE return #-}

    Compiler c >>= f = Compiler $ \r -> do
        res <- c r
        case res of
            CompilerDone x w    -> do
                res' <- unCompiler (f x) r
                return $ case res' of
                    CompilerDone y w'     -> CompilerDone y (w `mappend` w')
                    CompilerSnapshot s c' -> CompilerSnapshot s $ do
                        compilerTell w  -- Save dependencies!
                        c'
                    CompilerError e       -> CompilerError e
                    CompilerRequire i c'  -> CompilerRequire i $ do
                        compilerTell w  -- Save dependencies!
                        c'

            CompilerSnapshot s c' -> return $ CompilerSnapshot s (c' >>= f)
            CompilerError e       -> return $ CompilerError e
            CompilerRequire i c'  -> return $ CompilerRequire i (c' >>= f)
    {-# INLINE (>>=) #-}

    fail = compilerThrow . return
    {-# INLINE fail #-}


--------------------------------------------------------------------------------
instance Applicative Compiler where
    pure x = return x
    {-# INLINE pure #-}

    f <*> x = f >>= \f' -> fmap f' x
    {-# INLINE (<*>) #-}


--------------------------------------------------------------------------------
instance MonadMetadata Compiler where
    getMetadata = compilerGetMetadata
    getMatches  = compilerGetMatches


--------------------------------------------------------------------------------
instance MonadError [String] Compiler where
  throwError = compilerThrow
  catchError = compilerCatch


--------------------------------------------------------------------------------
runCompiler :: Compiler a -> CompilerRead -> IO (CompilerResult a)
runCompiler compiler read' = handle handler $ unCompiler compiler read'
  where
    handler :: SomeException -> IO (CompilerResult a)
    handler e = return $ CompilerError [show e]


--------------------------------------------------------------------------------
instance Alternative Compiler where
    empty   = compilerThrow []
    x <|> y = compilerCatch x $ \es -> do
        logger <- compilerLogger <$> compilerAsk
        forM_ es $ \e -> compilerUnsafeIO $ Logger.debug logger $
            "Hakyll.Core.Compiler.Internal: Alternative failed: " ++ e
        y
    {-# INLINE (<|>) #-}


--------------------------------------------------------------------------------
compilerAsk :: Compiler CompilerRead
compilerAsk = Compiler $ \r -> return $ CompilerDone r mempty
{-# INLINE compilerAsk #-}


--------------------------------------------------------------------------------
compilerTell :: CompilerWrite -> Compiler ()
compilerTell deps = Compiler $ \_ -> return $ CompilerDone () deps
{-# INLINE compilerTell #-}


--------------------------------------------------------------------------------
compilerThrow :: [String] -> Compiler a
compilerThrow es = Compiler $ \_ -> return $ CompilerError es
{-# INLINE compilerThrow #-}


--------------------------------------------------------------------------------
compilerCatch :: Compiler a -> ([String] -> Compiler a) -> Compiler a
compilerCatch (Compiler x) f = Compiler $ \r -> do
    res <- x r
    case res of
        CompilerDone res' w  -> return (CompilerDone res' w)
        CompilerSnapshot s c -> return (CompilerSnapshot s (compilerCatch c f))
        CompilerError e      -> unCompiler (f e) r
        CompilerRequire i c  -> return (CompilerRequire i (compilerCatch c f))
{-# INLINE compilerCatch #-}


--------------------------------------------------------------------------------
-- | Put the result back in a compiler
compilerResult :: CompilerResult a -> Compiler a
compilerResult x = Compiler $ \_ -> return x
{-# INLINE compilerResult #-}


--------------------------------------------------------------------------------
compilerUnsafeIO :: IO a -> Compiler a
compilerUnsafeIO io = Compiler $ \_ -> do
    x <- io
    return $ CompilerDone x mempty
{-# INLINE compilerUnsafeIO #-}


--------------------------------------------------------------------------------
compilerTellDependencies :: [Dependency] -> Compiler ()
compilerTellDependencies ds = do
  logger <- compilerLogger <$> compilerAsk
  forM_ ds $ \d -> compilerUnsafeIO $ Logger.debug logger $
      "Hakyll.Core.Compiler.Internal: Adding dependency: " ++ show d
  compilerTell mempty {compilerDependencies = ds}
{-# INLINE compilerTellDependencies #-}


--------------------------------------------------------------------------------
compilerTellCacheHits :: Int -> Compiler ()
compilerTellCacheHits ch = compilerTell mempty {compilerCacheHits = ch}
{-# INLINE compilerTellCacheHits #-}


--------------------------------------------------------------------------------
compilerGetMetadata :: Identifier -> Compiler Metadata
compilerGetMetadata identifier = do
    provider <- compilerProvider <$> compilerAsk
    compilerTellDependencies [IdentifierDependency identifier]
    compilerUnsafeIO $ resourceMetadata provider identifier


--------------------------------------------------------------------------------
compilerGetMatches :: Pattern -> Compiler [Identifier]
compilerGetMatches pattern = do
    universe <- compilerUniverse <$> compilerAsk
    let matching = filterMatches pattern $ S.toList universe
        set'     = S.fromList matching
    compilerTellDependencies [PatternDependency pattern set']
    return matching
