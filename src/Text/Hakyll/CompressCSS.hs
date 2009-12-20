module Text.Hakyll.CompressCSS
    ( compressCSS
    ) where

import Data.List
import Text.Regex

-- | subRegex with arguments flipped for easy function composition.
subRegex' :: String -> String -> String -> String
subRegex' pattern replacement str = subRegex (mkRegex pattern) str replacement

-- | Compress CSS to speed up your site.
compressCSS :: String -> String
compressCSS = compressSeparators
            . compressWhitespace
            . stripComments

-- | Compresses certain forms of separators.
compressSeparators :: String -> String
compressSeparators = subRegex' ";\\s*}" "}" 
                   . subRegex' "\\s*([{};:])\\s*" "\\1"

-- | Compresses all whitespace.
compressWhitespace :: String -> String
compressWhitespace = subRegex' "\\s\\s*" " "

-- | Function that strips CSS comments away.
stripComments :: String -> String
stripComments [] = []
stripComments str | isPrefixOf "/*" str = stripComments $ eatComments $ drop 2 str
                  | otherwise = (head str) : (stripComments $ tail str)
    where eatComments str' | null str' = []
                           | isPrefixOf "*/" str' = drop 2 str'
                           | otherwise = eatComments $ tail str'
