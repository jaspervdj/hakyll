--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Hakyll.Core.Compiler
    ( Compiler
    , getUnderlying
    , getUnderlyingExtension
    , makeItem
    , getRoute
    , getResourceBody
    , getResourceString
    , getResourceLBS
    , getResourceFilePath

    , Internal.Snapshot
    , saveSnapshot
    , Internal.load
    , Internal.loadSnapshot
    , Internal.loadBody
    , Internal.loadSnapshotBody
    , Internal.loadAll
    , Internal.loadAllSnapshots

    , cached
    , unsafeCompiler
    , debugCompiler
    , noResult
    , withErrorMessage
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                 (unless, when, (>=>))
import           Data.Binary                   (Binary)
import           Data.ByteString.Lazy          (ByteString)
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Typeable                 (Typeable)
import           System.Environment            (getProgName)
import           System.FilePath               (takeExtension)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import qualified Hakyll.Core.Compiler.Require  as Internal
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Logger            as Logger
import           Hakyll.Core.Provider
import           Hakyll.Core.Routes
import qualified Hakyll.Core.Store             as Store


--------------------------------------------------------------------------------
-- | Get the underlying identifier.
getUnderlying :: Compiler Identifier
getUnderlying = compilerUnderlying <$> compilerAsk


--------------------------------------------------------------------------------
-- | Get the extension of the underlying identifier. Returns something like
-- @".html"@
getUnderlyingExtension :: Compiler String
getUnderlyingExtension = takeExtension . toFilePath <$> getUnderlying


--------------------------------------------------------------------------------
-- | Create an item from the underlying identifier and a given value.
makeItem :: a -> Compiler (Item a)
makeItem x = do
    identifier <- getUnderlying
    return $ Item identifier x


--------------------------------------------------------------------------------
-- | Get the route for a specified item
getRoute :: Identifier -> Compiler (Maybe FilePath)
getRoute identifier = do
    provider <- compilerProvider <$> compilerAsk
    routes   <- compilerRoutes <$> compilerAsk
    -- Note that this makes us dependend on that identifier: when the metadata
    -- of that item changes, the route may change, hence we have to recompile
    (mfp, um) <- compilerUnsafeIO $ runRoutes routes provider identifier
    when um $ compilerTellDependencies [IdentifierDependency identifier]
    return mfp


--------------------------------------------------------------------------------
-- | Get the full contents of the matched source file as a string,
-- but without metadata preamble, if there was one.
getResourceBody :: Compiler (Item String)
getResourceBody = getResourceWith resourceBody


--------------------------------------------------------------------------------
-- | Get the full contents of the matched source file as a string.
getResourceString :: Compiler (Item String)
getResourceString = getResourceWith resourceString


--------------------------------------------------------------------------------
-- | Get the full contents of the matched source file as a lazy bytestring.
getResourceLBS :: Compiler (Item ByteString)
getResourceLBS = getResourceWith resourceLBS


--------------------------------------------------------------------------------
-- | Get the file path of the resource we are compiling
getResourceFilePath :: Compiler FilePath
getResourceFilePath = do
    provider <- compilerProvider   <$> compilerAsk
    id'      <- compilerUnderlying <$> compilerAsk
    return $ resourceFilePath provider id'


--------------------------------------------------------------------------------
-- | Overloadable function for 'getResourceString' and 'getResourceLBS'
getResourceWith :: (Provider -> Identifier -> IO a) -> Compiler (Item a)
getResourceWith reader = do
    provider <- compilerProvider   <$> compilerAsk
    id'      <- compilerUnderlying <$> compilerAsk
    let filePath = toFilePath id'
    if resourceExists provider id'
        then compilerUnsafeIO $ Item id' <$> reader provider id'
        else fail $ error' filePath
  where
    error' fp = "Hakyll.Core.Compiler.getResourceWith: resource " ++
        show fp ++ " not found"


--------------------------------------------------------------------------------
-- | Save a snapshot of the item. This function returns the same item, which
-- convenient for building '>>=' chains.
saveSnapshot :: (Binary a, Typeable a)
             => Internal.Snapshot -> Item a -> Compiler (Item a)
saveSnapshot snapshot item = do
    store  <- compilerStore <$> compilerAsk
    logger <- compilerLogger <$> compilerAsk
    compilerUnsafeIO $ do
        Logger.debug logger $ "Storing snapshot: " ++ snapshot
        Internal.saveSnapshot store snapshot item

    -- Signal that we saved the snapshot.
    Compiler $ \_ -> return $ CompilerSnapshot snapshot (return item)


--------------------------------------------------------------------------------
-- | Turn on caching for a compilation value to avoid recomputing it
-- on subsequent Hakyll runs.
-- The storage key consists of the underlying identifier of the compiled
-- ressource and the given name.
cached :: (Binary a, Typeable a)
       => String
       -> Compiler a
       -> Compiler a
cached name compiler = do
    id'      <- compilerUnderlying <$> compilerAsk
    store    <- compilerStore      <$> compilerAsk
    provider <- compilerProvider   <$> compilerAsk

    -- Give a better error message when the resource is not there at all.
    unless (resourceExists provider id') $ fail $ itDoesntEvenExist id'

    let modified = resourceModified provider id'
    if modified
        then do
            x <- compiler
            compilerUnsafeIO $ Store.set store [name, show id'] x
            return x
        else do
            compilerTellCacheHits 1
            x        <- compilerUnsafeIO $ Store.get store [name, show id']
            progName <- compilerUnsafeIO getProgName
            case x of Store.Found x' -> return x'
                      _              -> fail $ error' progName
  where
    error' progName =
        "Hakyll.Core.Compiler.cached: Cache corrupt! " ++
         "Try running: " ++ progName ++ " clean"

    itDoesntEvenExist id' =
        "Hakyll.Core.Compiler.cached: You are trying to (perhaps "    ++
        "indirectly) use `cached` on a non-existing resource: there " ++
        "is no file backing " ++ show id'


--------------------------------------------------------------------------------
-- | Run an IO computation without dependencies in a Compiler
unsafeCompiler :: IO a -> Compiler a
unsafeCompiler = compilerUnsafeIO


--------------------------------------------------------------------------------
-- | Fail so that it is treated as non-defined in an @\$if()\$@ branching
-- "Hakyll.Web.Template" macro, and alternative
-- 'Hakyll.Web.Template.Context.Context's are tried
--
-- @since 4.13.0
noResult :: String -> Compiler a
noResult = compilerNoResult . return


--------------------------------------------------------------------------------
-- | Prepend an error line to the error, if there is one.  This allows you to
-- add helpful context to error messages.
--
-- @since 4.13.0
withErrorMessage :: String -> Compiler a -> Compiler a
withErrorMessage x = do
    compilerTry >=> either (compilerResult . CompilerError . prepend) return
  where
    prepend (CompilationFailure  es) = CompilationFailure  (x `NonEmpty.cons` es)
    prepend (CompilationNoResult es) = CompilationNoResult (x : es)


--------------------------------------------------------------------------------
-- | Compiler for debugging purposes.
-- Passes a message to the debug logger that is printed in verbose mode.
debugCompiler :: String -> Compiler ()
debugCompiler msg = do
    logger <- compilerLogger <$> compilerAsk
    compilerUnsafeIO $ Logger.debug logger msg
