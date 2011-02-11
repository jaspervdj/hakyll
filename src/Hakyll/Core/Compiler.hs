-- | A Compiler manages targets and dependencies between targets.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Compiler
    ( Compiler
    , runCompiler
    , getIdentifier
    , getRoute
    , getRouteFor
    , getResourceString
    , fromDependency
    , require_
    , require
    , requireA
    , requireAll_
    , requireAll
    , requireAllA
    , cached
    , unsafeCompiler
    , mapCompiler
    ) where

import Prelude hiding ((.), id)
import Control.Arrow ((>>>), (&&&), arr)
import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Control.Category (Category, (.), id)
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
import Hakyll.Core.Rules.Internal
import Hakyll.Core.Routes

-- | Run a compiler, yielding the resulting target and it's dependencies. This
-- version of 'runCompilerJob' also stores the result
--
runCompiler :: Compiler () CompileRule  -- ^ Compiler to run
            -> Identifier               -- ^ Target identifier
            -> ResourceProvider         -- ^ Resource provider
            -> Routes                   -- ^ Route
            -> Store                    -- ^ Store
            -> Bool                     -- ^ Was the resource modified?
            -> IO CompileRule           -- ^ Resulting item
runCompiler compiler identifier provider routes store modified = do
    -- Run the compiler job
    result <- runCompilerJob compiler identifier provider routes store modified

    -- Inspect the result
    case result of
        -- In case we compiled an item, we will store a copy in the cache first,
        -- before we return control. This makes sure the compiled item can later
        -- be accessed by e.g. require.
        CompileRule (CompiledItem x) ->
            storeSet store "Hakyll.Core.Compiler.runCompiler" identifier x

        -- Otherwise, we do nothing here
        _ -> return ()

    return result

-- | Get the identifier of the item that is currently being compiled
--
getIdentifier :: Compiler a Identifier
getIdentifier = fromJob $ const $ CompilerM $ compilerIdentifier <$> ask

-- | Get the route we are using for this item
--
getRoute :: Compiler a (Maybe FilePath)
getRoute = getIdentifier >>> getRouteFor

-- | Get the route for a specified item
--
getRouteFor :: Compiler Identifier (Maybe FilePath)
getRouteFor = fromJob $ \identifier -> CompilerM $ do
    routes <- compilerRoutes <$> ask
    return $ runRoutes routes identifier

-- | Get the resource we are compiling as a string
--
getResourceString :: Compiler Resource String
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
                   ++ " not found in the cache, the cache might be corrupted or"
                   ++ " the item you are referring to might not exist"


-- | Variant of 'require' which drops the current value
--
require_ :: (Binary a, Typeable a, Writable a)
         => Identifier
         -> Compiler b a
require_ identifier =
    fromDependency identifier >>> fromJob (const $ getDependency identifier)

-- | Require another target. Using this function ensures automatic handling of
-- dependencies
--
require :: (Binary a, Typeable a, Writable a)
        => Identifier
        -> (b -> a -> c)
        -> Compiler b c
require identifier = requireA identifier . arr . uncurry

-- | Arrow-based variant of 'require'
--
requireA :: (Binary a, Typeable a, Writable a)
         => Identifier
         -> Compiler (b, a) c
         -> Compiler b c
requireA identifier = (id &&& require_ identifier >>>)

-- | Variant of 'requireAll' which drops the current value
--
requireAll_ :: (Binary a, Typeable a, Writable a)
            => Pattern
            -> Compiler b [a]
requireAll_ pattern = fromDependencies getDeps >>> fromJob requireAll_'
  where
    getDeps = matches pattern . resourceList
    requireAll_' = const $ CompilerM $ do
        deps <- getDeps . compilerResourceProvider <$> ask
        mapM (unCompilerM . getDependency) deps

-- | Require a number of targets. Using this function ensures automatic handling
-- of dependencies
--
requireAll :: (Binary a, Typeable a, Writable a)
           => Pattern
           -> (b -> [a] -> c)
           -> Compiler b c
requireAll pattern = requireAllA pattern . arr . uncurry

-- | Arrow-based variant of 'requireAll'
--
requireAllA :: (Binary a, Typeable a, Writable a)
            => Pattern
            -> Compiler (b, [a]) c
            -> Compiler b c
requireAllA pattern = (id &&& requireAll_ pattern >>>)

cached :: (Binary a, Typeable a, Writable a)
       => String
       -> Compiler Resource a
       -> Compiler Resource a
cached name (Compiler d j) = Compiler d $ const $ CompilerM $ do
    identifier <- compilerIdentifier <$> ask
    store <- compilerStore <$> ask
    modified <- compilerResourceModified <$> ask
    liftIO $ putStrLn $
        show identifier ++ ": " ++ if modified then "MODIFIED" else "OK"
    if modified
        then do v <- unCompilerM $ j Resource
                liftIO $ storeSet store name identifier v
                return v
        else do v <- liftIO $ storeGet store name identifier
                case v of Just v' -> return v'
                          Nothing -> error'
  where
    error' = error "Hakyll.Core.Compiler.cached: Cache corrupt!"

-- | Create an unsafe compiler from a function in IO
--
unsafeCompiler :: (a -> IO b)   -- ^ Function to lift
               -> Compiler a b  -- ^ Resulting compiler
unsafeCompiler f = fromJob $ CompilerM . liftIO . f

-- | Map over a compiler
--
mapCompiler :: Compiler a b
            -> Compiler [a] [b]
mapCompiler (Compiler d j) = Compiler d $ mapM j
