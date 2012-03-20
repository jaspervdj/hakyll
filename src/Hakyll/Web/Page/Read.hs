-- | Module providing a function to parse a page from a file
--
module Hakyll.Web.Page.Read
    ( readPage
    ) where

import Control.Applicative ((<$>), (<*>), (<*))
import qualified Data.Map as M

import Text.Parsec.Char (alphaNum, anyChar, char, newline, oneOf, string)
import Text.Parsec.Combinator (choice, many1, manyTill, option, skipMany1)
import Text.Parsec.Prim (many, parse, skipMany, (<?>))
import Text.Parsec.String (Parser)

import Hakyll.Core.Util.String
import Hakyll.Web.Page.Internal

-- | Space or tab, no newline
inlineSpace :: Parser Char
inlineSpace = oneOf ['\t', ' '] <?> "space"

-- | Parse a single metadata field
--
metadataField :: Parser (String, String)
metadataField = do
    key <- manyTill alphaNum $ char ':'
    skipMany1 inlineSpace <?> "space followed by metadata for: " ++ key
    value <- manyTill anyChar newline
    trailing' <- many trailing
    return (key, trim $ value ++ concat trailing')
  where
    trailing = (++) <$> many1 inlineSpace <*> manyTill anyChar newline

-- | Parse a metadata block, including delimiters and trailing newlines
--
metadata :: Parser [(String, String)]
metadata = do
    open <- many1 (char '-') <* many inlineSpace <* newline
    metadata' <- many metadataField
    _ <- choice $ map (string . replicate (length open)) ['-', '.']
    skipMany inlineSpace
    skipMany1 newline
    return metadata'

-- | Parse a Hakyll page
--
page :: Parser ([(String, String)], String)
page = do
    metadata' <- option [] metadata
    body <- many anyChar
    return (metadata', body)

readPage :: String -> Page String
readPage input = case parse page "page" input of
    Left err      -> error (show err)
    Right (md, b) -> Page (M.fromList md) b
