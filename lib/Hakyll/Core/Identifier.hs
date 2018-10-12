--------------------------------------------------------------------------------
-- | An identifier is a type used to uniquely identify an item. An identifier is
-- conceptually similar to a file path. Examples of identifiers are:
--
-- * @posts/foo.markdown@
--
-- * @index@
--
-- * @error/404@
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Identifier
    ( Identifier
    , fromFilePath
    , toFilePath
    , identifierVersion
    , setVersion
    ) where


--------------------------------------------------------------------------------
import           Control.DeepSeq     (NFData (..))
import           Data.List           (intercalate)
import           System.FilePath     (dropTrailingPathSeparator, splitPath)


--------------------------------------------------------------------------------
import           Data.Binary         (Binary (..))
import           Data.Typeable       (Typeable)
import           GHC.Exts            (IsString, fromString)


--------------------------------------------------------------------------------
data Identifier = Identifier
    { identifierVersion :: Maybe String
    , identifierPath    :: String
    } deriving (Eq, Ord, Typeable)


--------------------------------------------------------------------------------
instance Binary Identifier where
    put (Identifier v p) = put v >> put p
    get = Identifier <$> get <*> get


--------------------------------------------------------------------------------
instance IsString Identifier where
    fromString = fromFilePath


--------------------------------------------------------------------------------
instance NFData Identifier where
    rnf (Identifier v p) = rnf v `seq` rnf p `seq` ()


--------------------------------------------------------------------------------
instance Show Identifier where
    show i = case identifierVersion i of
        Nothing -> toFilePath i
        Just v  -> toFilePath i ++ " (" ++ v ++ ")"


--------------------------------------------------------------------------------
-- | Parse an identifier from a string
fromFilePath :: FilePath -> Identifier
fromFilePath = Identifier Nothing .
    intercalate "/" . filter (not . null) . split'
  where
    split' = map dropTrailingPathSeparator . splitPath


--------------------------------------------------------------------------------
-- | Convert an identifier to a relative 'FilePath'
toFilePath :: Identifier -> FilePath
toFilePath = identifierPath


--------------------------------------------------------------------------------
setVersion :: Maybe String -> Identifier -> Identifier
setVersion v i = i {identifierVersion = v}
