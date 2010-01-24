-- | Miscellaneous text manipulation functions.
module Text.Hakyll.Util 
    ( trim
    , stripHTML
    , link
    ) where

import Data.Char (isSpace)

-- | Trim a string (drop spaces, tabs and newlines at both sides).
trim :: String -> String
trim = reverse . trim' . reverse . trim'
  where
    trim' = dropWhile isSpace

-- | Strip html tags from the given string.
stripHTML :: String -> String
stripHTML []  = []
stripHTML str = let (beforeTag, rest) = break (== '<') str
                    (_, afterTag)     = break (== '>') rest
                in beforeTag ++ stripHTML (tail' afterTag)
  where
    -- We need a failsafe tail function.
    tail' [] = []
    tail' xs = tail xs

-- | Make a HTML link.
--
--   > link "foo" "bar.html" == "<a href='bar.html'>foo</a>"
link :: String -- ^ Link text.
     -> String -- ^ Link destination.
     -> String
link text destination = "<a href=\"" ++ destination ++ "\">"
                      ++ text ++ "</a>"
