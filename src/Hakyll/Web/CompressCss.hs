-- | Module used for CSS compression. The compression is currently in a simple
-- state, but would typically reduce the number of bytes by about 25%.
--
module Text.Hakyll.Internal.CompressCss
    ( compressCss
    ) where

import Data.Char (isSpace)
import Data.Maybe (listToMaybe)
import Data.List (isPrefixOf)
import Text.Regex.Posix ((=~~))

-- | A simple (but inefficient) regex replace funcion
--
replaceAll :: String              -- ^ Pattern
           -> (String -> String)  -- ^ Replacement (called on capture)
           -> String              -- ^ Source string
           -> String              -- ^ Result
replaceAll pattern f source =
    case listToMaybe (source =~~ pattern) of
        Nothing     -> source
        Just (o, l) ->
            let (before, tmp) = splitAt o source
                (capture, after) = splitAt l tmp
            in before ++ f capture ++ replaceAll pattern f after

-- | Compress CSS to speed up your site.
--
compressCss :: String -> String
compressCss = compressSeparators
            . stripComments
            . compressWhitespace

-- | Compresses certain forms of separators.
--
compressSeparators :: String -> String
compressSeparators = replaceAll "; *}" (const "}")
                   . replaceAll " *([{};:]) *" (take 1 . dropWhile isSpace)
                   . replaceAll ";;*" (const ";")

-- | Compresses all whitespace.
--
compressWhitespace :: String -> String
compressWhitespace = replaceAll "[ \t\n][ \t\n]*" (const " ")

-- | Function that strips CSS comments away.
--
stripComments :: String -> String
stripComments [] = []
stripComments str
    | isPrefixOf "/*" str = stripComments $ eatComments $ drop 2 str
    | otherwise = head str : stripComments (drop 1 str)
  where
    eatComments str' | null str' = []
                     | isPrefixOf "*/" str' = drop 2 str'
                     | otherwise = eatComments $ drop 1 str'
