--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
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
import           Control.Monad                  (forM)
import           Control.Monad.Fail             (MonadFail)
import           Data.Binary                    (Binary (..), getWord8,
                                                 putWord8, Get)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap              as KeyMap
import qualified Data.Aeson.Key                 as AK
#else
import qualified Data.HashMap.Strict            as KeyMap
#endif
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
lookupString key meta = KeyMap.lookup (keyFromString key) meta >>= Yaml.toString


--------------------------------------------------------------------------------
lookupStringList :: String -> Metadata -> Maybe [String]
lookupStringList key meta =
    KeyMap.lookup (keyFromString key) meta >>= Yaml.toList >>= mapM Yaml.toString


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
getMetadataField' :: (MonadFail m, MonadMetadata m) => Identifier -> String -> m String
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
                list = map (\(k, v) -> (keyToText k, BinaryYaml v)) $ KeyMap.toList obj
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
                    KeyMap.fromList $ map (\(k, v) -> (keyFromText k, unBinaryYaml v)) list

            1 -> do
                list <- get :: Get [BinaryYaml]
                return $ BinaryYaml $
                    Yaml.Array $ V.fromList $ map unBinaryYaml list

            2 -> BinaryYaml . Yaml.String <$> get
            3 -> BinaryYaml . Yaml.Number <$> get
            4 -> BinaryYaml . Yaml.Bool   <$> get
            5 -> return $ BinaryYaml Yaml.Null
            _ -> fail "Data.Binary.get: Invalid Binary Metadata"


--------------------------------------------------------------------------------
#if MIN_VERSION_aeson(2,0,0)
keyFromString :: String -> AK.Key
keyFromString = AK.fromString

keyToText :: AK.Key -> T.Text
keyToText = AK.toText

keyFromText :: T.Text -> AK.Key
keyFromText = AK.fromText
#else
keyFromString :: String -> T.Text
keyFromString = T.pack

keyToText :: T.Text -> T.Text
keyToText = id

keyFromText :: T.Text -> T.Text
keyFromText = id
#endif
