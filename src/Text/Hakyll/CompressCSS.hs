module Text.Hakyll.CompressCSS
    ( compressCSS
    ) where

import Data.List

-- | Compress CSS to speed up your site.
compressCSS :: String -> String
compressCSS = stripComments

-- | Function that strips CSS comments away.
stripComments :: String -> String
stripComments [] = []
stripComments str | isPrefixOf "/*" str = stripComments $ eatComments $ drop 2 str
                  | otherwise = (head str) : (stripComments $ tail str)
    where eatComments str' | null str' = []
                           | isPrefixOf "*/" str' = drop 2 str'
                           | otherwise = eatComments $ tail str'
