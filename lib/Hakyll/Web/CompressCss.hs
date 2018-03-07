--------------------------------------------------------------------------------
-- | Module used for CSS compression. The compression is currently in a simple
-- state, but would typically reduce the number of bytes by about 25%.
module Hakyll.Web.CompressCss
    ( compressCssCompiler
    , compressCss
    ) where


--------------------------------------------------------------------------------
import           Data.Char               (isSpace)
import           Data.List               (dropWhileEnd, isPrefixOf)


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
compressCss = withoutStrings (handleCalcExpressions compressSeparators . compressWhitespace)
            . dropWhileEnd isSpace
            . dropWhile isSpace
            . stripComments


--------------------------------------------------------------------------------
-- | Compresses certain forms of separators.
compressSeparators :: String -> String
compressSeparators =
    replaceAll "; *}" (const "}") .
    replaceAll ";+" (const ";") .
    replaceAll " *[{};,>+~!] *" (take 1 . dropWhile isSpace) .
    replaceAll ": *" (take 1) -- not destroying pseudo selectors (#323)

-- | Uses `compressCalcExpression` on all parenthesised calc expressions
-- and applies `transform` to all parts outside of them
handleCalcExpressions :: (String -> String) -> String -> String
handleCalcExpressions transform = top transform
  where
    top f ""                             = f ""
    top f str | "calc(" `isPrefixOf` str = f "calc" ++ nested 0 compressCalcExpression (drop 4 str)
    top f (x:xs)                         = top (f . (x:)) xs
    
    -- when called with depth=0, the first character must be a '('
    nested :: Int -> (String -> String) -> String -> String
    nested _     f ""                             = f "" -- shouldn't happen, mismatched nesting
    nested depth f str | "calc(" `isPrefixOf` str = nested depth f (drop 4 str)
    nested 1     f (')':xs)                       = f ")" ++ top transform xs
    nested depth f (x:xs)                         = nested (case x of
                                                      '(' -> depth + 1
                                                      ')' -> depth - 1 -- assert: depth > 1
                                                      _   -> depth
                                                    ) (f . (x:)) xs

-- | does not remove whitespace around + and -, which is important in calc() expressions
compressCalcExpression :: String -> String
compressCalcExpression =
    replaceAll " *[*/] *| *\\)|\\( *" (take 1 . dropWhile isSpace)

--------------------------------------------------------------------------------
-- | Compresses all whitespace.
compressWhitespace :: String -> String
compressWhitespace = replaceAll "[ \t\n\r]+" (const " ")

--------------------------------------------------------------------------------
-- | Function that strips CSS comments away (outside of strings).
stripComments :: String -> String
stripComments ""                       = ""
stripComments ('/':'*':str)            = stripComments $ eatComment str
stripComments (x:xs) | x `elem` "\"'"  = retainString x xs stripComments
                     | otherwise       = x : stripComments xs

eatComment :: String -> String
eatComment "" = ""
eatComment ('*':'/':str) = str
eatComment (_:str) = eatComment str


--------------------------------------------------------------------------------
-- | Helper functions to handle string tokens correctly.

-- TODO: handle backslash escapes
withoutStrings :: (String -> String) -> String -> String
withoutStrings f str = case span (`notElem` "\"'") str of
    (text, "")     -> f text
    (text, d:rest) -> f text ++ retainString d rest (withoutStrings f)

retainString :: Char -> String -> (String -> String) -> String
retainString delim str cont = case span (/= delim) str of
    (val, "")     -> delim : val
    (val, _:rest) -> delim : val ++ delim : cont rest