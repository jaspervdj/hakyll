--------------------------------------------------------------------------------
-- | Internally used compiler module
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Hakyll.Core.Compiler.Internal
    ( -- * Types
      Snapshot
    , CompilerRead (..)
    , CompilerWrite (..)
    , Reason (..)
    , CompilerResult (..)
    , Compiler (..)
    , runCompiler

      -- * Core operations
    , compilerResult
    , compilerTell
    , compilerAsk
    , compilerUnsafeIO

      -- * Error operations
    , compilerThrow
    , compilerFailBranch
    , compilerCatch
    , compilerTry
    , getReason

      -- * Utilities
    , compilerDebugEntries
    , compilerTellDependencies
    , compilerTellCacheHits
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            (Alternative (..))
import           Control.Exception              (SomeException, handle)
import           Control.Monad                  (forM_)
import           Control.Monad.Except           (MonadError (..))
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
      compilerLogger     :: Logger.Logger
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
-- | Distinguishes reasons in a 'CompilerError'
data Reason a
    -- | An exception occured during compilation
    = CompilationFailure a
    -- | Absence of any result, most notably in template contexts
    | NoCompilationResult a
    deriving Functor


-- | Unwrap a `Reason`
getReason :: Reason a -> a
getReason (CompilationFailure x)  = x
getReason (NoCompilationResult x) = x


--------------------------------------------------------------------------------
-- | An intermediate result of a compilation step
data CompilerResult a
    = CompilerDone a CompilerWrite
    | CompilerSnapshot Snapshot (Compiler a)
    | CompilerRequire (Identifier, Snapshot) (Compiler a)
    | CompilerError (Reason [String])


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
            CompilerRequire i c'  -> CompilerRequire i (fmap f c')
            CompilerError e       -> CompilerError e
    {-# INLINE fmap #-}


--------------------------------------------------------------------------------
instance Monad Compiler where
    return x = compilerResult $ CompilerDone x mempty
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
                    CompilerRequire i c'  -> CompilerRequire i $ do
                        compilerTell w  -- Save dependencies!
                        c'
                    CompilerError e       -> CompilerError e

            CompilerSnapshot s c' -> return $ CompilerSnapshot s (c' >>= f)
            CompilerRequire i c'  -> return $ CompilerRequire i (c' >>= f)
            CompilerError e       -> return $ CompilerError e
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
-- | Access provided metadata from anywhere
instance MonadMetadata Compiler where
    getMetadata = compilerGetMetadata
    getMatches  = compilerGetMatches


--------------------------------------------------------------------------------
-- | Compilation may fail with multiple error messages.
-- 'catchError' handles errors from 'throwError', 'fail' and 'Hakyll.Core.Compiler.failBranch'
instance MonadError [String] Compiler where
    throwError = compilerThrow
    catchError c = compilerCatch c . (. getReason)


--------------------------------------------------------------------------------
-- | Like 'unCompiler' but treating IO exceptions as 'CompilerError's
runCompiler :: Compiler a -> CompilerRead -> IO (CompilerResult a)
runCompiler compiler read' = handle handler $ unCompiler compiler read'
  where
    handler :: SomeException -> IO (CompilerResult a)
    handler e = return $ CompilerError $ CompilationFailure [show e]


--------------------------------------------------------------------------------
-- | Trying alternative compilers if the first fails, regardless whether through
-- 'fail', 'throwError' or 'Hakyll.Core.Compiler.failBranch'.
-- Aggregates error messages if all fail.
instance Alternative Compiler where
    empty   = compilerFailBranch []
    x <|> y = x `compilerCatch` (\rx -> y `compilerCatch` (\ry ->
        case (rx, ry) of
          (CompilationFailure xs,  CompilationFailure ys)  -> compilerThrow $ xs ++ ys
          (CompilationFailure xs,  NoCompilationResult ys) -> debug ys >> compilerThrow xs
          (NoCompilationResult xs, CompilationFailure ys)  -> debug xs >> compilerThrow ys
          (NoCompilationResult xs, NoCompilationResult ys) -> compilerFailBranch $ xs ++ ys
        ))
      where
        debug = compilerDebugEntries "Hakyll.Core.Compiler.Internal: Alternative fail suppressed"
    {-# INLINE (<|>) #-}


--------------------------------------------------------------------------------
-- | Put the result back in a compiler
compilerResult :: CompilerResult a -> Compiler a
compilerResult x = Compiler $ \_ -> return x
{-# INLINE compilerResult #-}


--------------------------------------------------------------------------------
-- | Get the current environment
compilerAsk :: Compiler CompilerRead
compilerAsk = Compiler $ \r -> return $ CompilerDone r mempty
{-# INLINE compilerAsk #-}


--------------------------------------------------------------------------------
-- | Put a 'CompilerWrite'
compilerTell :: CompilerWrite -> Compiler ()
compilerTell = compilerResult . CompilerDone ()
{-# INLINE compilerTell #-}


--------------------------------------------------------------------------------
-- | Run an IO computation without dependencies in a Compiler
compilerUnsafeIO :: IO a -> Compiler a
compilerUnsafeIO io = Compiler $ \_ -> do
    x <- io
    return $ CompilerDone x mempty
{-# INLINE compilerUnsafeIO #-}


--------------------------------------------------------------------------------
-- | Put a 'CompilerError' with  multiple error messages as 'CompilationFailure'
compilerThrow :: [String] -> Compiler a
compilerThrow = compilerResult . CompilerError . CompilationFailure


-- | Put a 'CompilerError' with  multiple messages as 'NoCompilationResult'
compilerFailBranch :: [String] -> Compiler a
compilerFailBranch = compilerResult . CompilerError . NoCompilationResult


--------------------------------------------------------------------------------
-- | Allows to distinguish 'CompilerError's and branch on them with 'Either'
-- 
-- prop> compilerTry = (`compilerCatch` return . Left) . fmap Right
compilerTry :: Compiler a -> Compiler (Either (Reason [String]) a)
compilerTry (Compiler x) = Compiler $ \r -> do
    res <- x r
    case res of
        CompilerDone res' w  -> return (CompilerDone (Right res') w)
        CompilerSnapshot s c -> return (CompilerSnapshot s (compilerTry c))
        CompilerRequire i c  -> return (CompilerRequire i (compilerTry c))
        CompilerError e      -> return (CompilerDone (Left e) mempty)
{-# INLINE compilerTry #-}


--------------------------------------------------------------------------------
-- | Allows you to recover from 'CompilerError's.
-- Uses the same parameter order as 'catchError' so that it can be used infix.
-- 
-- prop> c `compilerCatch` f = compilerTry c >>= either f return
compilerCatch :: Compiler a -> (Reason [String] -> Compiler a) -> Compiler a
compilerCatch (Compiler x) f = Compiler $ \r -> do
    res <- x r
    case res of
        CompilerDone res' w  -> return (CompilerDone res' w)
        CompilerSnapshot s c -> return (CompilerSnapshot s (compilerCatch c f))
        CompilerRequire i c  -> return (CompilerRequire i (compilerCatch c f))
        CompilerError e      -> unCompiler (f e) r
{-# INLINE compilerCatch #-}


--------------------------------------------------------------------------------
compilerDebugLog :: [String] -> Compiler ()
compilerDebugLog ms = do
  logger <- compilerLogger <$> compilerAsk
  compilerUnsafeIO $ forM_ ms $ Logger.debug logger

--------------------------------------------------------------------------------
-- | Pass a list of messages with a heading to the debug logger
compilerDebugEntries :: String -> [String] -> Compiler ()
compilerDebugEntries msg = compilerDebugLog . (msg:) . map indent
  where
    indent = unlines . map ("    "++) . lines


--------------------------------------------------------------------------------
compilerTellDependencies :: [Dependency] -> Compiler ()
compilerTellDependencies ds = do
  compilerDebugLog $ map (\d ->
      "Hakyll.Core.Compiler.Internal: Adding dependency: " ++ show d) ds
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
