--------------------------------------------------------------------------------
-- | Internally used compiler module
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Compiler.Internal
    ( CompilerRead (..)
    , Compiler
    , runCompiler
    , compilerTell
    , compilerAsk
    , compilerThrow
    , compilerCatch
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative          (Alternative (..),
                                               Applicative (..))
import           Data.Monoid                  (mappend, mempty)


--------------------------------------------------------------------------------
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Logger
import           Hakyll.Core.ResourceProvider
import           Hakyll.Core.Routes
import           Hakyll.Core.Store


--------------------------------------------------------------------------------
-- | Environment in which a compiler runs
data CompilerRead = CompilerRead
    { -- | Target identifier
      compilerIdentifier       :: Identifier
    , -- | Resource provider
      compilerResourceProvider :: ResourceProvider
    , -- | List of all known identifiers
      compilerUniverse         :: [Identifier]
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
type CompilerWrite = [Dependency]


--------------------------------------------------------------------------------
data CompilerResult a where
    CompilerDone    :: a -> CompilerWrite -> CompilerResult a
    CompilerError   :: String -> CompilerResult a
    CompilerRequire :: Identifier -> (b -> Compiler a) -> CompilerResult a


--------------------------------------------------------------------------------
newtype Compiler a = Compiler
    { unCompiler :: CompilerRead -> IO (CompilerResult a)
    }


--------------------------------------------------------------------------------
instance Functor Compiler where
    fmap f (Compiler c) = Compiler $ \r -> do
        res <- c r
        return $ case res of
            CompilerDone x w    -> CompilerDone (f x) w
            CompilerError e     -> CompilerError e
            CompilerRequire i g -> CompilerRequire i (\x -> fmap f (g x))
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
                    CompilerDone y w'   -> CompilerDone y (w `mappend` w')
                    CompilerError e     -> CompilerError e
                    CompilerRequire i g -> CompilerRequire i $ \z -> do
                        compilerTell w  -- Save dependencies!
                        g z

            CompilerError e     -> return $ CompilerError e
            CompilerRequire i g -> return $ CompilerRequire i $ \z -> g z >>= f
    {-# INLINE (>>=) #-}


--------------------------------------------------------------------------------
instance Applicative Compiler where
    pure x = return x
    {-# INLINE pure #-}

    f <*> x = f >>= \f' -> fmap f' x
    {-# INLINE (<*>) #-}


--------------------------------------------------------------------------------
runCompiler :: Compiler a -> CompilerRead -> IO (CompilerResult a)
runCompiler = unCompiler


--------------------------------------------------------------------------------
instance Alternative Compiler where
    empty   = compilerThrow "Hakyll.Core.Compiler.Internal: empty alternative"
    x <|> y = compilerCatch x (\_ -> y)
    {-# INLINE (<|>) #-}


--------------------------------------------------------------------------------
compilerAsk :: Compiler CompilerRead
compilerAsk = Compiler $ \r -> return $ CompilerDone r mempty
{-# INLINE compilerAsk #-}


--------------------------------------------------------------------------------
compilerTell :: [Dependency] -> Compiler ()
compilerTell deps = Compiler $ \_ -> return $ CompilerDone () deps
{-# INLINE compilerTell #-}


--------------------------------------------------------------------------------
compilerThrow :: String -> Compiler a
compilerThrow e = Compiler $ \_ -> return $ CompilerError e
{-# INLINE compilerThrow #-}


--------------------------------------------------------------------------------
compilerCatch :: Compiler a -> (String -> Compiler a) -> Compiler a
compilerCatch (Compiler x) f = Compiler $ \r -> do
    res <- x r
    case res of
        CompilerError e -> unCompiler (f e) r
        _               -> return res
{-# INLINE compilerCatch #-}
