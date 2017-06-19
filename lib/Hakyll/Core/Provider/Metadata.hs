--------------------------------------------------------------------------------
-- | Internal module to parse metadata
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
module Hakyll.Core.Provider.Metadata
    ( loadMetadata
    , parsePage

    , MetadataException (..)
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                 (second)
import           Control.Exception             (Exception, throwIO)
import           Control.Monad                 (guard)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.List.Extended            (breakWhen)
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Yaml                     as Yaml
import           Hakyll.Core.Identifier
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider.Internal
import           System.IO                     as IO


--------------------------------------------------------------------------------
loadMetadata :: Provider -> Identifier -> IO (Metadata, Maybe String)
loadMetadata p identifier = do
    hasHeader  <- probablyHasMetadataHeader fp
    (md, body) <- if hasHeader
        then second Just <$> loadMetadataHeader fp
        else return (mempty, Nothing)

    emd <- case mi of
        Nothing  -> return mempty
        Just mi' -> loadMetadataFile $ resourceFilePath p mi'

    return (md <> emd, body)
  where
    normal = setVersion Nothing identifier
    fp     = resourceFilePath p identifier
    mi     = M.lookup normal (providerFiles p) >>= resourceInfoMetadata


--------------------------------------------------------------------------------
loadMetadataHeader :: FilePath -> IO (Metadata, String)
loadMetadataHeader fp = do
    fileContent <- readFile fp
    case parsePage fileContent of
        Right x   -> return x
        Left  err -> throwIO $ MetadataException fp err


--------------------------------------------------------------------------------
loadMetadataFile :: FilePath -> IO Metadata
loadMetadataFile fp = do
    fileContent <- B.readFile fp
    let errOrMeta = Yaml.decodeEither' fileContent
    either (fail . show) return errOrMeta


--------------------------------------------------------------------------------
-- | Check if a file "probably" has a metadata header. The main goal of this is
-- to exclude binary files (which are unlikely to start with "---").
probablyHasMetadataHeader :: FilePath -> IO Bool
probablyHasMetadataHeader fp = do
    handle <- IO.openFile fp IO.ReadMode
    bs     <- BC.hGet handle 1024
    IO.hClose handle
    return $ isMetadataHeader bs
  where
    isMetadataHeader bs =
        let pre = BC.takeWhile (\x -> x /= '\n' && x /= '\r') bs
        in  BC.length pre >= 3 && BC.all (== '-') pre


--------------------------------------------------------------------------------
-- | Parse the page metadata and body.
splitMetadata :: String -> (Maybe String, String)
splitMetadata str0 = fromMaybe (Nothing, str0) $ do
    guard $ leading >= 3
    let !str1 = drop leading str0
    guard $ all isNewline (take 1 str1)
    let !(!meta, !content0) = breakWhen isTrailing str1
    guard $ not $ null content0
    let !content1 = drop (leading + 1) content0
        !content2 = dropWhile isNewline $ dropWhile isInlineSpace content1
    -- Adding this newline fixes the line numbers reported by the YAML parser.
    -- It's a bit ugly but it works.
    return (Just ('\n' : meta), content2)
  where
    -- Parse the leading "---"
    !leading = length $ takeWhile (== '-') str0

    -- Predicate to recognize the trailing "---" or "..."
    isTrailing []       = False
    isTrailing (x : xs) =
        isNewline x && length (takeWhile isDash xs) == leading

    -- Characters
    isNewline     c = c == '\n' || c == '\r'
    isDash        c = c == '-'  || c == '.'
    isInlineSpace c = c == '\t' || c == ' '


--------------------------------------------------------------------------------
parseMetadata :: String -> Either Yaml.ParseException Metadata
parseMetadata = Yaml.decodeEither' . T.encodeUtf8 . T.pack


--------------------------------------------------------------------------------
parsePage :: String -> Either Yaml.ParseException (Metadata, String)
parsePage fileContent = case mbMetaBlock of
    Nothing        -> return (mempty, content)
    Just metaBlock -> case parseMetadata metaBlock of
        Left  err  -> Left   err
        Right meta -> return (meta, content)
  where
    !(!mbMetaBlock, !content) = splitMetadata fileContent


--------------------------------------------------------------------------------
-- | Thrown in the IO monad if things go wrong. Provides a nice-ish error
-- message.
data MetadataException = MetadataException FilePath Yaml.ParseException


--------------------------------------------------------------------------------
instance Exception MetadataException


--------------------------------------------------------------------------------
instance Show MetadataException where
    show (MetadataException fp err) =
        fp ++ ": " ++ Yaml.prettyPrintParseException err ++ hint

      where
        hint = case err of
            Yaml.InvalidYaml (Just (Yaml.YamlParseException {..}))
                | yamlProblem == problem -> "\n" ++
                    "Hint: if the metadata value contains characters such\n" ++
                    "as ':' or '-', try enclosing it in quotes."
            _ -> ""

        problem = "mapping values are not allowed in this context"
