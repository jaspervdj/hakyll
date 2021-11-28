--------------------------------------------------------------------------------
-- | An identifier is a type used to uniquely identify an item. An identifier
-- conceptually corresponds to a file path. Examples of identifiers are:
--
-- * @posts/foo.markdown@
--
-- * @index@
--
-- * @error/404@
--
-- See 'Identifier' for details.

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
import           System.FilePath     (dropTrailingPathSeparator, splitPath,
                                      pathSeparator, normalise)


--------------------------------------------------------------------------------
import           Data.Binary         (Binary (..))
import           Data.Typeable       (Typeable)
import           GHC.Exts            (IsString, fromString)


--------------------------------------------------------------------------------
{- | A key data type to identify a compiled 'Hakyll.Core.Item.Item' in the 'Hakyll.Core.Store.Store'.
Conceptually, it's a combination of a file path and a version name.
The version is used only when a file is
compiled within a rule using the 'version' wrapper function
(the same source file
can be compiled into several items in the store, so the version exists to distinguish
them).
Use functions like 'fromFilePath', 'setVersion', 'Hakyll.Core.Metadata.getMatches' to build an 'Identifier'.

=== __Usage Examples__
Normally, compiled items are saved to the store by 'Hakyll.Core.Rules.Rules' with an automatic, implicit identifier
and loaded from the store by the user in another rule with a manual, explicit identifier.

__Identifiers when using match__.
Using 'Hakyll.Core.Rules.match' builds an implicit identifier that corresponds to the expanded, relative path
of the source file on disk (relative to the project directory configured
with 'Hakyll.Core.Configuration.providerDirectory'):

@
-- e.g. file on disk: 'posts\/hakyll.md'
match "posts/*" $ do                                      -- saved with implicit identifier 'posts\/hakyll.md'
    compile pandocCompiler

match "about/*" $ do
    compiledPost <- load (fromFilePath "posts/hakyll.md") -- load with explicit identifier
    ...
@

__Identifiers when using create__.
Using 'Hakyll.Core.Rules.create' (thereby inventing a file path with no underlying file on disk)
builds an implicit identifier that corresponds to the invented file path:

@
create ["index.html"] $ do                            -- saved with implicit identifier 'index.html'
    makeItem ("Hello world" :: String)

match "about/*" $ do
    compiledIndex <- load (fromFilePath "index.html") -- load with an explicit identifier
    ...
@

__Identifiers when using versions__.
With 'Hakyll.Core.Rules.version' the same file can compiled into several items on the store.
A version name is needed to distinguish them:

@
-- e.g. file on disk: 'posts\/hakyll.md'
match "posts/*" $ do                                  -- saved with implicit identifier ('posts\/hakyll.md', no-version)
    compile $ pandocCompiler

match "posts/*" $ version "raw" $ do                  -- saved with implicit identifier ('posts\/hakyll.md', version 'raw')
    compile getResourceBody

match "about/*" $ do
    compiledPost <- load (fromFilePath "posts/hakyll.md")                      -- load no-version version
    rawPost <- load . setVersion (Just "raw") $ fromFilePath "posts/hakyll.md" -- load version 'raw'
    ...
@
Use 'setVersion' to specify or replace the version of an identifier like @fromFilePath "posts/hakyll.md"@.
-}
data Identifier = Identifier
    { identifierVersion :: Maybe String
    , identifierPath    :: String
    } deriving (Eq, Ord, Typeable)


--------------------------------------------------------------------------------
instance Binary Identifier where
    put (Identifier v p) = put v >> put p
    get = Identifier <$> get <*> get


--------------------fromFilePath "posts/hakyll.md"------------------------------------------------------------
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
fromFilePath = Identifier Nothing . normalise


--------------------------------------------------------------------------------
-- | Convert an identifier to a relative 'FilePath'
toFilePath :: Identifier -> FilePath
toFilePath = normalise . identifierPath


--------------------------------------------------------------------------------
setVersion :: Maybe String -> Identifier -> Identifier
setVersion v i = i {identifierVersion = v}
