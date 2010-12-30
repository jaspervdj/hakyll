-- | A Compiler manages targets and dependencies between targets.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Compiler
    ( Compiler
    , getIdentifier
    , getResourceString
    , require
    , requireAll
    ) where

import Prelude hiding ((.), id)
import Control.Arrow ((>>>))
import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Control.Category (Category, (.))

import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.CompiledItem
import Hakyll.Core.Writable
import Hakyll.Core.ResourceProvider
import Hakyll.Core.Compiler.Internal

getIdentifier :: Compiler a Identifier
getIdentifier = fromJob $ const $ CompilerM $
    compilerIdentifier <$> ask

getResourceString :: Compiler a String
getResourceString = getIdentifier >>> getResourceString'
  where
    getResourceString' = fromJob $ \id' -> CompilerM $ do
        provider <- compilerResourceProvider <$> ask
        liftIO $ resourceString provider id'

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
    require' x = CompilerM $ do
        lookup' <- compilerDependencyLookup <$> ask
        return $ f x $ unCompiledItem $ lookup' identifier

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
        lookup' <- compilerDependencyLookup <$> ask
        return $ f x $ map (unCompiledItem . lookup') deps
