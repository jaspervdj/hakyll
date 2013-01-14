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
    , getResourceWith

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
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           ((<$>))
import           Data.Binary                   (Binary)
import           Data.ByteString.Lazy          (ByteString)
import           Data.Typeable                 (Typeable)
import           System.Environment            (getProgName)
import           System.FilePath               (takeExtension)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import qualified Hakyll.Core.Compiler.Require  as Internal
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
makeItem :: a -> Compiler (Item a)
makeItem x = do
    identifier <- getUnderlying
    return $ Item identifier x


--------------------------------------------------------------------------------
-- | Get the route for a specified item
getRoute :: Identifier -> Compiler (Maybe FilePath)
getRoute identifier = do
    routes <- compilerRoutes <$> compilerAsk
    return $ runRoutes routes identifier


--------------------------------------------------------------------------------
-- | Get the body of the underlying resource
getResourceBody :: Compiler (Item String)
getResourceBody = getResourceWith resourceBody


--------------------------------------------------------------------------------
-- | Get the resource we are compiling as a string
getResourceString :: Compiler (Item String)
getResourceString = getResourceWith resourceString


--------------------------------------------------------------------------------
-- | Get the resource we are compiling as a lazy bytestring
getResourceLBS :: Compiler (Item ByteString)
getResourceLBS = getResourceWith resourceLBS


--------------------------------------------------------------------------------
-- | Overloadable function for 'getResourceString' and 'getResourceLBS'
getResourceWith :: (Provider -> Identifier -> IO a) -> Compiler (Item a)
getResourceWith reader = do
    provider <- compilerProvider   <$> compilerAsk
    id'      <- compilerUnderlying <$> compilerAsk
    let filePath = toFilePath id'
    if resourceExists provider id'
        then compilerUnsafeIO $ Item id' <$> reader provider id'
        else compilerThrow $ error' filePath
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
        return item


--------------------------------------------------------------------------------
cached :: (Binary a, Typeable a)
       => String
       -> Compiler a
       -> Compiler a
cached name compiler = do
    id'      <- compilerUnderlying <$> compilerAsk
    store    <- compilerStore      <$> compilerAsk
    provider <- compilerProvider   <$> compilerAsk
    modified <- compilerUnsafeIO $ resourceModified provider id'
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
                      _              -> compilerThrow (error' progName)
  where
    error' progName =
        "Hakyll.Core.Compiler.cached: Cache corrupt! " ++
         "Try running: " ++ progName ++ " clean"


--------------------------------------------------------------------------------
unsafeCompiler :: IO a -> Compiler a
unsafeCompiler = compilerUnsafeIO


--------------------------------------------------------------------------------
-- | Compiler for debugging purposes
debugCompiler :: String -> Compiler ()
debugCompiler msg = do
    logger <- compilerLogger <$> compilerAsk
    compilerUnsafeIO $ Logger.debug logger msg
