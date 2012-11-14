--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Hakyll.Core.Compiler
    ( Compiler
    , getIdentifier
    , getRoute
    , getRouteFor
    , getMetadata
    , getMetadataFor
    , getResourceBody
    , getResourceString
    , getResourceLBS
    , getResourceWith
    , require
    , requireAll
    , cached
    , unsafeCompiler
    , debugCompiler
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           ((<$>))
import           Data.Binary                   (Binary)
import           Data.ByteString.Lazy          (ByteString)
import           Data.Typeable                 (Typeable)
import           Prelude                       hiding (id, (.))
import           System.Environment            (getProgName)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Compiler.Require
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Logger            as Logger
import           Hakyll.Core.Metadata
import           Hakyll.Core.ResourceProvider
import           Hakyll.Core.Routes
import qualified Hakyll.Core.Store             as Store
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
-- | Get the identifier of the item that is currently being compiled
getIdentifier :: Compiler Identifier
getIdentifier = compilerIdentifier <$> compilerAsk


--------------------------------------------------------------------------------
-- | Get the route we are using for this item
getRoute :: Compiler (Maybe FilePath)
getRoute = getIdentifier >>= getRouteFor


--------------------------------------------------------------------------------
-- | Get the route for a specified item
getRouteFor :: Identifier -> Compiler (Maybe FilePath)
getRouteFor identifier = do
    routes <- compilerRoutes <$> compilerAsk
    return $ runRoutes routes identifier


--------------------------------------------------------------------------------
getMetadata :: Compiler Metadata
getMetadata = getIdentifier >>= getMetadataFor


--------------------------------------------------------------------------------
getMetadataFor :: Identifier -> Compiler Metadata
getMetadataFor identifier = do
    provider <- compilerProvider <$> compilerAsk
    compilerTellDependencies [IdentifierDependency identifier]
    compilerUnsafeIO $ resourceMetadata provider identifier


--------------------------------------------------------------------------------
-- | Get the body of the underlying resource
getResourceBody :: Compiler String
getResourceBody = getResourceWith resourceBody


--------------------------------------------------------------------------------
-- | Get the resource we are compiling as a string
getResourceString :: Compiler String
getResourceString = getResourceWith $ const resourceString


--------------------------------------------------------------------------------
-- | Get the resource we are compiling as a lazy bytestring
--
getResourceLBS :: Compiler ByteString
getResourceLBS = getResourceWith $ const resourceLBS


--------------------------------------------------------------------------------
-- | Overloadable function for 'getResourceString' and 'getResourceLBS'
getResourceWith :: (ResourceProvider -> Identifier -> IO a) -> Compiler a
getResourceWith reader = do
    provider <- compilerProvider   <$> compilerAsk
    id'      <- compilerIdentifier <$> compilerAsk
    let filePath = toFilePath id'
    if resourceExists provider id'
        then compilerUnsafeIO $ reader provider id'
        else compilerThrow $ error' filePath
  where
    error' fp = "Hakyll.Core.Compiler.getResourceWith: resource " ++
        show fp ++ " not found"


--------------------------------------------------------------------------------
cached :: (Binary a, Typeable a, Writable a)
       => String
       -> Compiler a
       -> Compiler a
cached name compiler = do
    id'      <- compilerIdentifier <$> compilerAsk
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
