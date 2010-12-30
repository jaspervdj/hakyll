-- | Miscellaneous string manipulation functions.
--
module Hakyll.Web.Util.String
    ( trim
    , toSiteRoot
    ) where

import Data.Char (isSpace)

import System.FilePath (splitPath, takeDirectory, joinPath)

-- | Trim a string (drop spaces, tabs and newlines at both sides).
--
trim :: String -> String
trim = reverse . trim' . reverse . trim'
  where
    trim' = dropWhile isSpace

-- | Get the relative url to the site root, for a given (absolute) url
--
toSiteRoot :: FilePath -> FilePath
toSiteRoot = emptyException . joinPath . map parent . splitPath . takeDirectory
  where
    parent = const ".."
    emptyException [] = "."
    emptyException x  = x
