-- | A Compiler manages targets and dependencies between targets.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Compiler
    ( Compiler
    , runCompiler
    , getIdentifier
    , getRoute
    , getResourceString
    , waitFor
    , require
    , requireAll
    , cached
    ) where

import Prelude hiding ((.), id)
import Control.Arrow ((>>>))
import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Control.Category (Category, (.))
import Data.Maybe (fromMaybe)

import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.CompiledItem
import Hakyll.Core.Writable
import Hakyll.Core.ResourceProvider
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Store

-- | Run a compiler, yielding the resulting target and it's dependencies. This
-- version of 'runCompilerJob' also stores the result
--
runCompiler :: Compiler () CompiledItem  -- ^ Compiler to run
            -> Identifier                -- ^ Target identifier
            -> ResourceProvider          -- ^ Resource provider
            -> Maybe FilePath            -- ^ Route
            -> Store                     -- ^ Store
            -> Bool                      -- ^ Was the resource modified?
            -> IO CompiledItem           -- ^ Resulting item
runCompiler compiler identifier provider route store modified = do
    -- Run the compiler job
    CompiledItem result <- runCompilerJob
        compiler identifier provider route store modified

    -- Store a copy in the cache and return
    storeSet store "Hakyll.Core.Compiler.runCompiler" identifier result
    return $ CompiledItem result

-- | Get the identifier of the item that is currently being compiled
--
getIdentifier :: Compiler a Identifier
getIdentifier = fromJob $ const $ CompilerM $ compilerIdentifier <$> ask

-- | Get the route we are using for this item
--
getRoute :: Compiler a (Maybe FilePath)
getRoute = fromJob $ const $ CompilerM $ compilerRoute <$> ask

-- | Get the resource we are compiling as a string
--
getResourceString :: Compiler a String
getResourceString = getIdentifier >>> getResourceString'
  where
    getResourceString' = fromJob $ \id' -> CompilerM $ do
        provider <- compilerResourceProvider <$> ask
        liftIO $ resourceString provider id'

-- | Auxiliary: get a dependency
--
getDependency :: (Binary a, Writable a, Typeable a)
                      => Identifier -> CompilerM a
getDependency identifier = CompilerM $ do
    store <- compilerStore <$> ask
    fmap (fromMaybe error') $ liftIO $
        storeGet store "Hakyll.Core.Compiler.runCompiler" identifier
  where
    error' = error $  "Hakyll.Core.Compiler.getDependency: "
                   ++ show identifier
                   ++ " not found in the cache, the cache might be corrupted"

-- | Wait until another compiler has finished before running this compiler
--
waitFor :: Identifier -> Compiler a a
waitFor = fromDependencies . const . return

-- | Require another target. Using this function ensures automatic handling of
-- dependencies
--
require :: (Binary a, Typeable a, Writable a)
        => Identifier
        -> (b -> a -> c)
        -> Compiler b c
require identifier f =
    waitFor identifier >>> fromJob require'
  where
    require' x = do
        y <- getDependency identifier
        return $ f x y

-- | Require a number of targets. Using this function ensures automatic handling
-- of dependencies
--
requireAll :: (Binary a, Typeable a, Writable a)
           => Pattern
           -> (b -> [a] -> c)
           -> Compiler b c
requireAll pattern f =
    fromDependencies getDeps >>> fromJob requireAll'
  where
    getDeps = matches pattern . resourceList
    requireAll' x = CompilerM $ do
        deps <- getDeps . compilerResourceProvider <$> ask
        items <- mapM (unCompilerM . getDependency) deps
        return $ f x items

cached :: (Binary a, Typeable a, Writable a)
       => String
       -> Compiler () a
       -> Compiler () a
cached name (Compiler d j) = Compiler d $ const $ CompilerM $ do
    identifier <- compilerIdentifier <$> ask
    store <- compilerStore <$> ask
    modified <- compilerResourceModified <$> ask
    liftIO $ putStrLn $
        show identifier ++ ": " ++ if modified then "MODIFIED" else "OK"
    if modified
        then do v <- unCompilerM $ j ()
                liftIO $ storeSet store name identifier v
                return v
        else do v <- liftIO $ storeGet store name identifier
                case v of Just v' -> return v'
                          Nothing -> error'
  where
    error' = error "Hakyll.Core.Compiler.cached: Cache corrupt!"
