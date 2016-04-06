--------------------------------------------------------------------------------
module Hakyll.Core.Metadata
    ( Metadata
    , lookupString
    , lookupStringList

    , MonadMetadata (..)
    , getMetadataField
    , getMetadataField'
    , makePatternDependency

    , BinaryMetadata (..)
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                  (second)
import           Control.Monad                  (forM)
import           Data.Binary                    (Binary (..), getWord8,
                                                 putWord8, Get)
import qualified Data.HashMap.Strict            as HMS
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Data.Yaml.Extended                      as Yaml
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern


--------------------------------------------------------------------------------
type Metadata = Yaml.Object


--------------------------------------------------------------------------------
lookupString :: String -> Metadata -> Maybe String
lookupString key meta = HMS.lookup (T.pack key) meta >>= Yaml.toString


--------------------------------------------------------------------------------
lookupStringList :: String -> Metadata -> Maybe [String]
lookupStringList key meta =
    HMS.lookup (T.pack key) meta >>= Yaml.toList >>= mapM Yaml.toString


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
    return $ lookupString key metadata


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


--------------------------------------------------------------------------------
-- | Newtype wrapper for serialization.
newtype BinaryMetadata = BinaryMetadata
    {unBinaryMetadata :: Metadata}


instance Binary BinaryMetadata where
    put (BinaryMetadata obj) = put (BinaryYaml $ Yaml.Object obj)
    get = do
        BinaryYaml (Yaml.Object obj) <- get
        return $ BinaryMetadata obj


--------------------------------------------------------------------------------
newtype BinaryYaml = BinaryYaml {unBinaryYaml :: Yaml.Value}


--------------------------------------------------------------------------------
instance Binary BinaryYaml where
    put (BinaryYaml yaml) = case yaml of
        Yaml.Object obj -> do
            putWord8 0
            let list :: [(T.Text, BinaryYaml)]
                list = map (second BinaryYaml) $ HMS.toList obj
            put list

        Yaml.Array arr -> do
            putWord8 1
            let list = map BinaryYaml (V.toList arr) :: [BinaryYaml]
            put list

        Yaml.String s -> putWord8 2 >> put s
        Yaml.Number n -> putWord8 3 >> put n
        Yaml.Bool   b -> putWord8 4 >> put b
        Yaml.Null     -> putWord8 5

    get = do
        tag <- getWord8
        case tag of
            0 -> do
                list <- get :: Get [(T.Text, BinaryYaml)]
                return $ BinaryYaml $ Yaml.Object $
                    HMS.fromList $ map (second unBinaryYaml) list

            1 -> do
                list <- get :: Get [BinaryYaml]
                return $ BinaryYaml $
                    Yaml.Array $ V.fromList $ map unBinaryYaml list

            2 -> BinaryYaml . Yaml.String <$> get
            3 -> BinaryYaml . Yaml.Number <$> get
            4 -> BinaryYaml . Yaml.Bool   <$> get
            5 -> return $ BinaryYaml Yaml.Null
            _ -> fail "Data.Binary.get: Invalid Binary Metadata"
