module Text.Hakyll.CompressCSS
    ( compressCSS
    ) where

import Data.List (isPrefixOf)
import Text.Hakyll.Regex (substitute)

-- | Compress CSS to speed up your site.
compressCSS :: String -> String
compressCSS = compressSeparators
            . stripComments
            . compressWhitespace

-- | Compresses certain forms of separators.
compressSeparators :: String -> String
compressSeparators = substitute "; *}" "}" 
                   . substitute " *([{};:]) *" "\\1"
                   . substitute ";;*" ";"

-- | Compresses all whitespace.
compressWhitespace :: String -> String
compressWhitespace = substitute "[ \t\n][ \t\n]*" " "

-- | Function that strips CSS comments away.
stripComments :: String -> String
stripComments [] = []
stripComments str | isPrefixOf "/*" str = stripComments $ eatComments $ drop 2 str
                  | otherwise = (head str) : (stripComments $ tail str)
    where eatComments str' | null str' = []
                           | isPrefixOf "*/" str' = drop 2 str'
                           | otherwise = eatComments $ tail str'
