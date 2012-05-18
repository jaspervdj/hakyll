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
-- >         => Identifier a
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
-- > Identifier a
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
-- Note that require will fetch a previously compiled item: in our example of
-- the type @a@. It is /very/ important that the compiler which produced this
-- value, produced the right type as well!
--
{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Hakyll.Core.Compiler
    ( Compiler
    , runCompiler
    , getIdentifier
    , getResource
    , getRoute
    , getRouteFor
    , getResourceString
    , getResourceLBS
    , getResourceWith
    , fromDependency
    , require_
    , require
    , requireA
    , requireAll_
    , requireAll
    , requireAllA
    , cached
    , unsafeCompiler
    , traceShowCompiler
    , mapCompiler
    , timedCompiler
    , byPattern
    , byExtension
    ) where

import Prelude hiding ((.), id)
import Control.Arrow ((>>>), (&&&), arr, first)
import Control.Applicative ((<$>))
import Control.Exception (SomeException, handle)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Control.Monad.Error (throwError)
import Control.Category (Category, (.), id)
import Data.List (find)
import System.FilePath (takeExtension)

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Data.ByteString.Lazy (ByteString)

import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.CompiledItem
import Hakyll.Core.Writable
import Hakyll.Core.Resource
import Hakyll.Core.Resource.Provider
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Store
import Hakyll.Core.Rules.Internal
import Hakyll.Core.Routes
import Hakyll.Core.Logger

-- | Run a compiler, yielding the resulting target and it's dependencies. This
-- version of 'runCompilerJob' also stores the result
--
runCompiler :: Compiler () CompileRule    -- ^ Compiler to run
            -> Identifier ()              -- ^ Target identifier
            -> ResourceProvider           -- ^ Resource provider
            -> [Identifier ()]            -- ^ Universe
            -> Routes                     -- ^ Route
            -> Store                      -- ^ Store
            -> Bool                       -- ^ Was the resource modified?
            -> Logger                     -- ^ Logger
            -> IO (Throwing CompileRule)  -- ^ Resulting item
runCompiler compiler id' provider universe routes store modified logger = do
    -- Run the compiler job
    result <- handle (\(e :: SomeException) -> return $ Left $ show e) $
        runCompilerJob compiler id' provider universe routes store modified
            logger

    -- Inspect the result
    case result of
        -- In case we compiled an item, we will store a copy in the cache first,
        -- before we return control. This makes sure the compiled item can later
        -- be accessed by e.g. require.
        Right (CompileRule (CompiledItem x)) ->
            storeSet store "Hakyll.Core.Compiler.runCompiler"
                     (castIdentifier id') x

        -- Otherwise, we do nothing here
        _ -> return ()

    return result

-- | Get the identifier of the item that is currently being compiled
--
getIdentifier :: Compiler a (Identifier b)
getIdentifier = fromJob $ const $ CompilerM $
    castIdentifier . compilerIdentifier <$> ask

-- | Get the resource that is currently being compiled
--
getResource :: Compiler a Resource
getResource = getIdentifier >>> arr fromIdentifier

-- | Get the route we are using for this item
--
getRoute :: Compiler a (Maybe FilePath)
getRoute = getIdentifier >>> getRouteFor

-- | Get the route for a specified item
--
getRouteFor :: Compiler (Identifier a) (Maybe FilePath)
getRouteFor = fromJob $ \identifier -> CompilerM $ do
    routes <- compilerRoutes <$> ask
    return $ runRoutes routes identifier

-- | Get the resource we are compiling as a string
--
getResourceString :: Compiler Resource String
getResourceString = getResourceWith resourceString

-- | Get the resource we are compiling as a lazy bytestring
--
getResourceLBS :: Compiler Resource ByteString
getResourceLBS = getResourceWith resourceLBS

-- | Overloadable function for 'getResourceString' and 'getResourceLBS'
--
getResourceWith :: (ResourceProvider -> Resource -> IO a)
                -> Compiler Resource a
getResourceWith reader = fromJob $ \r -> CompilerM $ do
    let filePath = unResource r
    provider <- compilerResourceProvider <$> ask
    if resourceExists provider r
        then liftIO $ reader provider r
        else throwError $ error' filePath
  where
    error' id' =  "Hakyll.Core.Compiler.getResourceWith: resource "
               ++ show id' ++ " not found"

-- | Auxiliary: get a dependency
--
getDependency :: (Binary a, Writable a, Typeable a)
              => Identifier a -> CompilerM a
getDependency id' = CompilerM $ do
    store <- compilerStore <$> ask
    result <- liftIO $ storeGet store "Hakyll.Core.Compiler.runCompiler" id'
    case result of
        NotFound      -> throwError notFound
        WrongType e r -> throwError $ wrongType e r
        Found x       -> return x
  where
    notFound =
        "Hakyll.Core.Compiler.getDependency: " ++ show id' ++ " was " ++
        "not found in the cache, the cache might be corrupted or " ++
        "the item you are referring to might not exist"
    wrongType e r =
        "Hakyll.Core.Compiler.getDependency: " ++ show id' ++ " was found " ++
        "in the cache, but does not have the right type: expected " ++ show e ++
        " but got " ++ show r

-- | Variant of 'require' which drops the current value
--
require_ :: (Binary a, Typeable a, Writable a)
         => Identifier a
         -> Compiler b a
require_ identifier =
    fromDependency identifier >>> fromJob (const $ getDependency identifier)

-- | Require another target. Using this function ensures automatic handling of
-- dependencies
--
require :: (Binary a, Typeable a, Writable a)
        => Identifier a
        -> (b -> a -> c)
        -> Compiler b c
require identifier = requireA identifier . arr . uncurry

-- | Arrow-based variant of 'require'
--
requireA :: (Binary a, Typeable a, Writable a)
         => Identifier a
         -> Compiler (b, a) c
         -> Compiler b c
requireA identifier = (id &&& require_ identifier >>>)

-- | Variant of 'requireAll' which drops the current value
--
requireAll_ :: (Binary a, Typeable a, Writable a)
            => Pattern a
            -> Compiler b [a]
requireAll_ pattern = fromDependencies (const getDeps) >>> fromJob requireAll_'
  where
    getDeps = map castIdentifier . filterMatches pattern . map castIdentifier
    requireAll_' = const $ CompilerM $ do
        deps <- getDeps . compilerUniverse <$> ask
        mapM (unCompilerM . getDependency) deps

-- | Require a number of targets. Using this function ensures automatic handling
-- of dependencies
--
requireAll :: (Binary a, Typeable a, Writable a)
           => Pattern a
           -> (b -> [a] -> c)
           -> Compiler b c
requireAll pattern = requireAllA pattern . arr . uncurry

-- | Arrow-based variant of 'requireAll'
--
requireAllA :: (Binary a, Typeable a, Writable a)
            => Pattern a
            -> Compiler (b, [a]) c
            -> Compiler b c
requireAllA pattern = (id &&& requireAll_ pattern >>>)

cached :: (Binary a, Typeable a, Writable a)
       => String
       -> Compiler Resource a
       -> Compiler Resource a
cached name (Compiler d j) = Compiler d $ const $ CompilerM $ do
    logger <- compilerLogger <$> ask
    identifier <- castIdentifier . compilerIdentifier <$> ask
    store <- compilerStore <$> ask
    modified <- compilerResourceModified <$> ask
    report logger $ "Checking cache: " ++ if modified then "modified" else "OK"
    if modified
        then do v <- unCompilerM $ j $ fromIdentifier identifier
                liftIO $ storeSet store name identifier v
                return v
        else do v <- liftIO $ storeGet store name identifier
                case v of Found v' -> return v'
                          _        -> throwError error'
  where
    error' = "Hakyll.Core.Compiler.cached: Cache corrupt!"

-- | Create an unsafe compiler from a function in IO
--
unsafeCompiler :: (a -> IO b)   -- ^ Function to lift
               -> Compiler a b  -- ^ Resulting compiler
unsafeCompiler f = fromJob $ CompilerM . liftIO . f

-- | Compiler for debugging purposes
--
traceShowCompiler :: Show a => Compiler a a
traceShowCompiler = fromJob $ \x -> CompilerM $ do
    logger <- compilerLogger <$> ask
    report logger $ show x
    return x

-- | Map over a compiler
--
mapCompiler :: Compiler a b
            -> Compiler [a] [b]
mapCompiler (Compiler d j) = Compiler d $ mapM j

-- | Log and time a compiler
--
timedCompiler :: String        -- ^ Message
              -> Compiler a b  -- ^ Compiler to time
              -> Compiler a b  -- ^ Resulting compiler
timedCompiler msg (Compiler d j) = Compiler d $ \x -> CompilerM $ do
    logger <- compilerLogger <$> ask
    timed logger msg $ unCompilerM $ j x

-- | Choose a compiler by identifier
--
-- For example, assume that most content files need to be compiled
-- normally, but a select few need an extra step in the pipeline:
--
-- > compile $ pageCompiler >>> byPattern id
-- >     [ ("projects.md", addProjectListCompiler)
-- >     , ("sitemap.md", addSiteMapCompiler)
-- >     ]
--
byPattern :: Compiler a b                  -- ^ Default compiler
          -> [(Pattern (), Compiler a b)]  -- ^ Choices
          -> Compiler a b                  -- ^ Resulting compiler
byPattern defaultCompiler choices = Compiler deps job
  where
    -- Lookup the compiler, give an error when it is not found
    lookup' identifier = maybe defaultCompiler snd $
        find (\(p, _) -> matches p identifier) choices
    -- Collect the dependencies of the choice
    deps = do
        identifier <- castIdentifier . dependencyIdentifier <$> ask
        compilerDependencies $ lookup' identifier
    -- Collect the job of the choice
    job x = CompilerM $ do
        identifier <- castIdentifier . compilerIdentifier <$> ask
        unCompilerM $ compilerJob (lookup' identifier) x

-- | Choose a compiler by extension
--
-- Example:
--
-- > match "css/*" $ do
-- >   route $ setExtension "css"
-- >   compile $ byExtension (error "Not a (S)CSS file")
-- >             [ (".css",  compressCssCompiler)
-- >             , (".scss", sass)
-- >             ]
--
-- This piece of code will select the @compressCssCompiler@ for @.css@ files,
-- and the @sass@ compiler (defined elsewhere) for @.scss@ files.
--
byExtension :: Compiler a b              -- ^ Default compiler
            -> [(String, Compiler a b)]  -- ^ Choices
            -> Compiler a b              -- ^ Resulting compiler
byExtension defaultCompiler = byPattern defaultCompiler . map (first extPattern)
  where
    extPattern c = predicate $ (== c) . takeExtension . toFilePath
