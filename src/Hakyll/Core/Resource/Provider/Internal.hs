--------------------------------------------------------------------------------
module Hakyll.Core.Resource.Provider.Internal
    ( ResourceProvider (..)
    , newResourceProvider

    , resourceList
    , resourceExists
    , resourceMetadataResource

    , resourceString
    , resourceLBS
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   ((<$>))
import qualified Data.ByteString.Lazy  as BL
import           Data.IORef
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Set              (Set)
import qualified Data.Set              as S
import           System.FilePath       (addExtension)


--------------------------------------------------------------------------------
import           Hakyll.Core.Resource
import           Hakyll.Core.Store
import           Hakyll.Core.Util.File


--------------------------------------------------------------------------------
-- | Responsible for retrieving and listing resources
data ResourceProvider = ResourceProvider
    { -- | A list of all files found
      resourceSet           :: Set Resource
    , -- | Cache keeping track of modified files
      resourceModifiedCache :: IORef (Map Resource Bool)
    , -- | Underlying persistent store for caching
      resourceStore         :: Store
    }


--------------------------------------------------------------------------------
-- | Create a resource provider
newResourceProvider :: Store                -- ^ Store to use
                    -> (FilePath -> Bool)   -- ^ Should we ignore this file?
                    -> FilePath             -- ^ Search directory
                    -> IO ResourceProvider  -- ^ Resulting provider
newResourceProvider store ignore directory = do
    list  <- map resource . filter (not . ignore) <$>
        getRecursiveContents False directory
    cache <- newIORef M.empty
    return $ ResourceProvider (S.fromList list) cache store


--------------------------------------------------------------------------------
resourceList :: ResourceProvider -> [Resource]
resourceList = S.toList . resourceSet


--------------------------------------------------------------------------------
-- | Check if a given resiyrce exists
resourceExists :: ResourceProvider -> Resource -> Bool
resourceExists provider = (`S.member` resourceSet provider)


--------------------------------------------------------------------------------
-- | Each resource may have an associated metadata resource (with a @.metadata@
-- filename)
resourceMetadataResource :: Resource -> Resource
resourceMetadataResource = resource . flip addExtension "metadata" . unResource


--------------------------------------------------------------------------------
-- | Get the raw body of a resource as string
resourceString :: Resource -> IO String
resourceString = readFile . unResource


--------------------------------------------------------------------------------
-- | Get the raw body of a resource of a lazy bytestring
resourceLBS :: Resource -> IO BL.ByteString
resourceLBS = BL.readFile . unResource
