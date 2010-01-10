module Text.Hakyll.CompressCSS
    ( compressCSS
    ) where

import Data.List (isPrefixOf)
import Text.Hakyll.Regex (substituteRegex)

-- | Compress CSS to speed up your site.
compressCSS :: String -> String
compressCSS = compressSeparators
            . stripComments
            . compressWhitespace

-- | Compresses certain forms of separators.
compressSeparators :: String -> String
compressSeparators = substituteRegex "; *}" "}" 
                   . substituteRegex " *([{};:]) *" "\\1"
                   . substituteRegex ";;*" ";"

-- | Compresses all whitespace.
compressWhitespace :: String -> String
compressWhitespace = substituteRegex "[ \t\n][ \t\n]*" " "

-- | Function that strips CSS comments away.
stripComments :: String -> String
stripComments [] = []
stripComments str | isPrefixOf "/*" str = stripComments $ eatComments $ drop 2 str
                  | otherwise = (head str) : (stripComments $ tail str)
    where eatComments str' | null str' = []
                           | isPrefixOf "*/" str' = drop 2 str'
                           | otherwise = eatComments $ tail str'
