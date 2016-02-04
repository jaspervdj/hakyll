--------------------------------------------------------------------------------
-- | Internal module to parse metadata
module Hakyll.Core.Provider.Metadata
    ( loadMetadata
    , metadata
    , page

      -- This parser can be reused in some places
    , metadataKey
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Arrow                 (second)
import qualified Data.ByteString.Char8         as BC
import           Data.Char                     (toLower)
import           Data.List                     (intercalate)
import qualified Data.Map                      as M
import           System.IO                     as IO
import           Text.Parsec                   ((<?>))
import qualified Text.Parsec                   as P
import           Text.Parsec.String            (Parser)


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider.Internal
import           Hakyll.Core.Util.Parser
import           Hakyll.Core.Util.String


--------------------------------------------------------------------------------
loadMetadata :: Provider -> Identifier -> IO (Metadata, Maybe String)
loadMetadata p identifier = do
    hasHeader  <- probablyHasMetadataHeader fp
    (md, body) <- if hasHeader
        then second Just <$> loadMetadataHeader fp
        else return (M.empty, Nothing)

    emd <- case mi of
        Nothing  -> return M.empty
        Just mi' -> loadMetadataFile $ resourceFilePath p mi'

    return (M.union md emd, body)
  where
    normal = setVersion Nothing identifier
    fp     = resourceFilePath p identifier
    mi     = M.lookup normal (providerFiles p) >>= resourceInfoMetadata


--------------------------------------------------------------------------------
loadMetadataHeader :: FilePath -> IO (Metadata, String)
loadMetadataHeader fp = do
    contents <- readFile fp
    case P.parse page fp contents of
        Left err      -> error (show err)
        Right (md, b) -> return (M.fromList md, b)


--------------------------------------------------------------------------------
loadMetadataFile :: FilePath -> IO Metadata
loadMetadataFile fp = do
    contents <- readFile fp
    case P.parse metadata fp contents of
        Left err  -> error (show err)
        Right md  -> return $ M.fromList md


--------------------------------------------------------------------------------
-- | Check if a file "probably" has a metadata header. The main goal of this is
-- to exclude binary files (which are unlikely to start with "---" or "#+").
probablyHasMetadataHeader :: FilePath -> IO Bool
probablyHasMetadataHeader fp = do
    handle <- IO.openFile fp IO.ReadMode
    bs     <- BC.hGet handle 1024
    IO.hClose handle
    return $ isMetadataHeader bs || isOrgMetadataHeader bs
  where
    isMetadataHeader bs =
        let pre = BC.takeWhile (\x -> x /= '\n' && x /= '\r') bs
        in  BC.length pre >= 3 && BC.all (== '-') pre
    isOrgMetadataHeader bs =
        (BC.pack "#+") `BC.isPrefixOf` bs


--------------------------------------------------------------------------------
-- | Space or tab, no newline
inlineSpace :: Parser Char
inlineSpace = P.oneOf ['\t', ' '] <?> "space"


--------------------------------------------------------------------------------
-- | Parse Windows newlines as well (i.e. "\n" or "\r\n")
newline :: Parser String
newline = P.string "\n" <|> P.string "\r\n"


--------------------------------------------------------------------------------
-- | Parse a blank line (only space characters and the final newline).
blankline :: Parser Char
blankline = P.skipMany1 inlineSpace *> P.newline

--------------------------------------------------------------------------------
-- | Parse a single metadata field
metadataField :: Parser (String, String)
metadataField = do
    key <- metadataKey
    _   <- P.char ':'
    P.skipMany1 inlineSpace <?> "space followed by metadata for: " ++ key
    value     <- P.manyTill P.anyChar newline
    trailing' <- P.many trailing
    return (key, trim $ intercalate " " $ value : trailing')
  where
    trailing = P.many1 inlineSpace *> P.manyTill P.anyChar newline


--------------------------------------------------------------------------------
-- | Parse a metadata block
metadata :: Parser [(String, String)]
metadata = P.many metadataField


--------------------------------------------------------------------------------
-- | Parse a metadata block, including delimiters and trailing newlines
metadataBlock :: Parser [(String, String)]
metadataBlock = do
    open      <- P.many1 (P.char '-') <* P.many inlineSpace <* newline
    metadata' <- metadata
    _         <- P.choice $ map (P.string . replicate (length open)) ['-', '.']
    P.skipMany inlineSpace
    P.skipMany1 newline
    return metadata'


--------------------------------------------------------------------------------
-- | Parse a org-style metadata block, including trailing newlines
orgMetadataBlock :: Parser [(String, String)]
orgMetadataBlock = P.try $ P.many1 orgMetaline <* P.skipMany blankline


--------------------------------------------------------------------------------
-- | Parse a org-style metadata definition
orgMetaline :: Parser (String, String)
orgMetaline = P.try $ P.string "#+" *> ((,) <$> key <*> value)
  where
    key   = map toLower <$> P.many1 P.alphaNum
                        <* P.char ':'
                        <* P.skipMany inlineSpace
    value = P.manyTill P.anyChar newline


--------------------------------------------------------------------------------
-- | Parse a page consisting of a metadata header and a body
page :: Parser ([(String, String)], String)
page = do
    metadata' <- P.option [] (orgMetadataBlock <|> metadataBlock)
    body      <- P.many P.anyChar
    return (metadata', body)
