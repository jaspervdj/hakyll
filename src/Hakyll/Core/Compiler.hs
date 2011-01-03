-- | A Compiler manages targets and dependencies between targets.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Compiler
    ( Compiler
    , getIdentifier
    , getRoute
    , getResourceString
    , storeResult
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

-- | Store a finished item in the cache
--
storeResult :: Store -> Identifier -> CompiledItem -> IO ()
storeResult store identifier (CompiledItem x) =
    storeSet store "Hakyll.Core.Compiler.storeResult" identifier x

-- | Auxiliary: get a dependency
--
getDependencyOrResult :: (Binary a, Writable a, Typeable a)
                      => Identifier -> CompilerM a
getDependencyOrResult identifier = CompilerM $ do
    lookup' <- compilerDependencyLookup <$> ask
    store <- compilerStore <$> ask
    case lookup' identifier of
        -- Found in the dependency lookup
        Just r  -> return $ unCompiledItem r
        -- Not found here, try the main cache
        Nothing -> fmap (fromMaybe error') $ liftIO $
            storeGet store "Hakyll.Core.Compiler.storeResult" identifier
  where
    error' = error "Hakyll.Core.Compiler.getDependency: Not found"

-- | Require another target. Using this function ensures automatic handling of
-- dependencies
--
require :: (Binary a, Typeable a, Writable a)
        => Identifier
        -> (b -> a -> c)
        -> Compiler b c
require identifier f =
    fromDependencies (const [identifier]) >>> fromJob require'
  where
    require' x = do
        y <- getDependencyOrResult identifier
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
        items <- mapM (unCompilerM . getDependencyOrResult) deps
        return $ f x items

cached :: (Binary a)
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
