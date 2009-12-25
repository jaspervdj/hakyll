module Text.Hakyll.Util 
    ( trim,
      split,
      stripHTML
    ) where

import Data.Char
import Data.List
import Text.Regex

-- | Trim a string (drop spaces and tabs at both sides).
trim :: String -> String
trim = reverse . trim' . reverse . trim'
    where trim' = dropWhile isSpace

-- | Strip html tags.
stripHTML :: String -> String
stripHTML []  = []
stripHTML str = let (beforeTag, rest) = break (== '<') str
                    (_, afterTag)     = break (== '>') rest
                in beforeTag ++ (stripHTML $ tail' afterTag)
    -- We need a failsafe tail function.
    where tail' [] = []
          tail' xs = tail xs

-- | Split a list at a certain element.
split :: String -> String -> [String]
split pattern = filter (not . null)
              . splitRegex (mkRegex pattern)
