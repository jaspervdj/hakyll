--------------------------------------------------------------------------------
-- | Module used for CSS compression. The compression is currently in a simple
-- state, but would typically reduce the number of bytes by about 25%.
module Hakyll.Web.CompressCss
    ( compressCssCompiler
    , compressCss
    ) where


--------------------------------------------------------------------------------
import           Data.Char               (isSpace)
import           Data.List               (isPrefixOf)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Core.Util.String


--------------------------------------------------------------------------------
-- | Compiler form of 'compressCss'
compressCssCompiler :: Compiler (Item String)
compressCssCompiler = fmap compressCss <$> getResourceString


--------------------------------------------------------------------------------
-- | Compress CSS to speed up your site.
compressCss :: String -> String
compressCss = compressSeparators . stripComments . compressWhitespace


--------------------------------------------------------------------------------
-- | Compresses certain forms of separators.
compressSeparators :: String -> String
compressSeparators [] = []
compressSeparators str
    | isPrefixOf "\"" str = head str : retainConstants compressSeparators "\"" (drop 1 str)
    | isPrefixOf "'" str = head str : retainConstants compressSeparators "'" (drop 1 str)
    | stripFirst = compressSeparators (drop 1 str)
    | stripSecond = compressSeparators (head str : (drop 2 str))
    | otherwise = head str : compressSeparators (drop 1 str)
  where
    prefix p = isPrefixOf p str
    stripFirst = or $ map prefix ["  ", " {", " }", ";;", ";}"]
    stripSecond = or $ map prefix ["{ ", "} ", "; "]

--------------------------------------------------------------------------------
-- | Compresses all whitespace.
compressWhitespace :: String -> String
compressWhitespace [] = []
compressWhitespace str
    | isPrefixOf "\"" str = head str : retainConstants compressWhitespace "\"" (drop 1 str)
    | isPrefixOf "'" str = head str : retainConstants compressWhitespace "'" (drop 1 str)
    | replaceOne = compressWhitespace (' ' : (drop 1 str))
    | replaceTwo = compressWhitespace (' ' : (drop 2 str))
    | otherwise = head str : compressWhitespace (drop 1 str)
  where
    prefix p = isPrefixOf p str
    replaceOne = or $ map prefix ["\t", "\n", "\r"]
    replaceTwo = or $ map prefix [" \t", " \n", " \r", "  "]

--------------------------------------------------------------------------------
-- | Function that strips CSS comments away.
stripComments :: String -> String
stripComments [] = []
stripComments str
    | isPrefixOf "\"" str = head str : retainConstants stripComments "\"" (drop 1 str)
    | isPrefixOf "'" str = head str : retainConstants stripComments "'" (drop 1 str)
    | isPrefixOf "/*" str = stripComments $ eatComments $ drop 2 str
    | otherwise = head str : stripComments (drop 1 str)
  where
    eatComments str'
        | null str' = []
        | isPrefixOf "*/" str' = drop 2 str'
        | otherwise = eatComments $ drop 1 str'

--------------------------------------------------------------------------------
-- | Helper function to handle string constants correctly.
retainConstants :: (String -> String) -> String -> String -> String
retainConstants f delim str
    | null str = []
    | isPrefixOf delim str = head str : f (drop 1 str)
    | otherwise = head str : retainConstants f delim (drop 1 str)
