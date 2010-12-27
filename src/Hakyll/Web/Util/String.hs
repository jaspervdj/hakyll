-- | Miscellaneous string manipulation functions.
--
module Hakyll.Web.Util.String
    ( trim
    ) where

import Data.Char (isSpace)

-- | Trim a string (drop spaces, tabs and newlines at both sides).
--
trim :: String -> String
trim = reverse . trim' . reverse . trim'
  where
    trim' = dropWhile isSpace
