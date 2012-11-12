--------------------------------------------------------------------------------
module Hakyll.Core.ResourceProvider.Internal
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
import           Hakyll.Core.Store
import           Hakyll.Core.Util.File
import           Hakyll.Core.Identifier


--------------------------------------------------------------------------------
-- | Responsible for retrieving and listing resources
data ResourceProvider = ResourceProvider
    { -- | A list of all files found
      resourceSet           :: Set (Identifier ())
    , -- | Cache keeping track of modified files
      resourceModifiedCache :: IORef (Map (Identifier ()) Bool)
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
    list  <- map fromFilePath . filter (not . ignore) <$>
        getRecursiveContents False directory
    cache <- newIORef M.empty
    return $ ResourceProvider (S.fromList list) cache store


--------------------------------------------------------------------------------
resourceList :: ResourceProvider -> [Identifier ()]
resourceList = S.toList . resourceSet


--------------------------------------------------------------------------------
-- | Check if a given resource exists
resourceExists :: ResourceProvider -> Identifier a -> Bool
resourceExists provider =
    (`S.member` resourceSet provider) . setVersion Nothing . castIdentifier


--------------------------------------------------------------------------------
-- | Each resource may have an associated metadata resource (with a @.metadata@
-- filename)
resourceMetadataResource :: Identifier a -> Identifier ()
resourceMetadataResource =
    fromFilePath . flip addExtension "metadata" . toFilePath


--------------------------------------------------------------------------------
-- | Get the raw body of a resource as string
resourceString :: Identifier a -> IO String
resourceString = readFile . toFilePath


--------------------------------------------------------------------------------
-- | Get the raw body of a resource of a lazy bytestring
resourceLBS :: Identifier a -> IO BL.ByteString
resourceLBS = BL.readFile . toFilePath
