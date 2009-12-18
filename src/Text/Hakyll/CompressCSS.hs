module Text.Hakyll.CompressCSS
    ( compressCSS
    ) where

import Data.List
import Text.Regex

-- | Compress CSS to speed up your site.
compressCSS :: String -> String
compressCSS = compressSeparators
            . compressWhitespace
            . stripComments

-- | Compresses certain forms of separators.
compressSeparators :: String -> String
compressSeparators str = subRegex (mkRegex "\\s*([;:])\\s*") str "\\1"

-- | Compresses all whitespace.
compressWhitespace :: String -> String
compressWhitespace str = subRegex (mkRegex "\\s\\s*") str " "

-- | Function that strips CSS comments away.
stripComments :: String -> String
stripComments [] = []
stripComments str | isPrefixOf "/*" str = stripComments $ eatComments $ drop 2 str
                  | otherwise = (head str) : (stripComments $ tail str)
    where eatComments str' | null str' = []
                           | isPrefixOf "*/" str' = drop 2 str'
                           | otherwise = eatComments $ tail str'
