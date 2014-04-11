--------------------------------------------------------------------------------
module Hakyll.Core.Metadata
    ( Metadata
    , MonadMetadata (..)
    , getMetadataField
    , getMetadataField'
    , makePatternDependency
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                  (forM)
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import qualified Data.Set                       as S


--------------------------------------------------------------------------------
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern


--------------------------------------------------------------------------------
type Metadata = Map String String


--------------------------------------------------------------------------------
class Monad m => MonadMetadata m where
    getMetadata    :: Identifier -> m Metadata
    getMatches     :: Pattern -> m [Identifier]

    getAllMetadata :: Pattern -> m [(Identifier, Metadata)]
    getAllMetadata pattern = do
        matches' <- getMatches pattern
        forM matches' $ \id' -> do
            metadata <- getMetadata id'
            return (id', metadata)


--------------------------------------------------------------------------------
getMetadataField :: MonadMetadata m => Identifier -> String -> m (Maybe String)
getMetadataField identifier key = do
    metadata <- getMetadata identifier
    return $ M.lookup key metadata


--------------------------------------------------------------------------------
-- | Version of 'getMetadataField' which throws an error if the field does not
-- exist.
getMetadataField' :: MonadMetadata m => Identifier -> String -> m String
getMetadataField' identifier key = do
    field <- getMetadataField identifier key
    case field of
        Just v  -> return v
        Nothing -> fail $ "Hakyll.Core.Metadata.getMetadataField': " ++
            "Item " ++ show identifier ++ " has no metadata field " ++ show key


--------------------------------------------------------------------------------
makePatternDependency :: MonadMetadata m => Pattern -> m Dependency
makePatternDependency pattern = do
    matches' <- getMatches pattern
    return $ PatternDependency pattern (S.fromList matches')
