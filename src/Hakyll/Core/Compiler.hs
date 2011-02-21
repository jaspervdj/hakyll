-- | A Compiler manages targets and dependencies between targets
--
-- The most distinguishing property of a 'Compiler' is that it is an Arrow. A
-- compiler of the type @Compiler a b@ is simply a compilation phase which takes
-- an @a@ as input, and produces a @b@ as output.
--
-- Compilers are chained using the '>>>' arrow operation. If we have a compiler
--
-- > getResourceString :: Compiler Resource String
--
-- which reads the resource, and a compiler
--
-- > readPage :: Compiler String (Page String)
--
-- we can chain these two compilers to get a
--
-- > (getResourceString >>> readPage) :: Compiler Resource (Page String)
--
-- Most compilers can be created by combining smaller compilers using '>>>'.
--
-- More advanced constructions are also possible using arrow, and sometimes
-- these are needed. For a good introduction to arrow, you can refer to
--
-- <http://en.wikibooks.org/wiki/Haskell/Understanding_arrows>
--
-- A construction worth writing a few paragraphs about here are the 'require'
-- functions. Different variants of this function are exported here, but they
-- all serve more or less the same goal.
--
-- When you use only '>>>' to chain your compilers, you get a linear pipeline --
-- it is not possible to add extra items from other compilers along the way.
-- This is where the 'require' functions come in.
--
-- This function allows you to reference other items, which are then added to
-- the pipeline. Let's look at this crappy ASCII illustration which represents
-- a pretty common scenario:
--
-- > read resource >>> pandoc render >>> layout >>> relativize URL's
-- >
-- > @templates/fancy.html@
--
-- We want to construct a pipeline of compilers to go from our resource to a
-- proper webpage. However, the @layout@ compiler takes more than just the
-- rendered page as input: it needs the @templates/fancy.html@ template as well.
--
-- This is an example of where we need the @require@ function. We can solve
-- this using a construction that looks like:
--
-- > ... >>> pandoc render >>> require >>> layout >>> ...
-- >                              |
-- > @templates/fancy.html@ ------/
--
-- This illustration can help us understand the type signature of 'require'.
--
-- > require :: (Binary a, Typeable a, Writable a)
-- >         => Identifier
-- >         -> (b -> a -> c)
-- >         -> Compiler b c
--
-- Let's look at it in detail:
--
-- > (Binary a, Typeable a, Writable a)
-- 
-- These are constraints for the @a@ type. @a@ (the template) needs to have
-- certain properties for it to be required.
--
-- > Identifier
--
-- This is simply @templates/fancy.html@: the 'Identifier' of the item we want
-- to 'require', in other words, the name of the item we want to add to the
-- pipeline somehow.
--
-- > (b -> a -> c)
--
-- This is a function given by the user, specifying /how/ the two items shall be
-- merged. @b@ is the output of the previous compiler, and @a@ is the item we
-- just required -- the template. This means @c@ will be the final output of the
-- 'require' combinator.
--
-- > Compiler b c
--
-- Indeed, we have now constructed a compiler which takes a @b@ and produces a
-- @c@. This means that we have a linear pipeline again, thanks to the 'require'
-- function. So, the 'require' function actually helps to reduce to complexity
-- of Hakyll applications!
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
getResourceString = fromJob $ \resource -> CompilerM $ do
    provider <- compilerResourceProvider <$> ask
    liftIO $ resourceString provider resource

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
    getDeps = matches pattern . map unResource . resourceList
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
    if modified
        then do v <- unCompilerM $ j $ Resource identifier
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
