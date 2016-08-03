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
compressCss = compressSeparators . compressWhitespace . stripComments


--------------------------------------------------------------------------------
-- | Compresses certain forms of separators.
compressSeparators :: String -> String
compressSeparators =
    replaceAll "; *}" (const "}") .
    replaceAll " *([{};,>+~]) *" (take 1 . dropWhile isSpace) .
    replaceAll "(:) *" (take 1) . -- not destroying pseudo selectors (#323)
    replaceAll ";+" (const ";")


--------------------------------------------------------------------------------
-- | Compresses all whitespace.
compressWhitespace :: String -> String
compressWhitespace = replaceAll "[ \t\n\r]+" (const " ") . dropWhile isSpace


--------------------------------------------------------------------------------
-- | Function that strips CSS comments away.
stripComments :: String -> String
stripComments [] = []
stripComments str
    | isPrefixOf "/*" str = stripComments $ eatComments $ drop 2 str
    | otherwise = head str : stripComments (drop 1 str)
  where
    eatComments str'
        | null str' = []
        | isPrefixOf "*/" str' = drop 2 str'
        | otherwise = eatComments $ drop 1 str'
