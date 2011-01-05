-- | Miscellaneous string manipulation functions.
--
module Hakyll.Web.Util.String
    ( trim
    , replaceAll
    , splitAll
    , toUrl
    , toSiteRoot
    ) where

import Data.Char (isSpace)
import Data.Maybe (listToMaybe)

import System.FilePath (splitPath, takeDirectory, joinPath)
import Text.Regex.PCRE ((=~~))

-- | Trim a string (drop spaces, tabs and newlines at both sides).
--
trim :: String -> String
trim = reverse . trim' . reverse . trim'
  where
    trim' = dropWhile isSpace

-- | A simple (but inefficient) regex replace funcion
--
replaceAll :: String              -- ^ Pattern
           -> (String -> String)  -- ^ Replacement (called on capture)
           -> String              -- ^ Source string
           -> String              -- ^ Result
replaceAll pattern f source = replaceAll' source
  where
    replaceAll' src = case listToMaybe (src =~~ pattern) of
        Nothing     -> src
        Just (o, l) ->
            let (before, tmp) = splitAt o src
                (capture, after) = splitAt l tmp
            in before ++ f capture ++ replaceAll' after

-- | A simple regex split function. The resulting list will contain no empty
-- strings.
--
splitAll :: String    -- ^ Pattern
         -> String    -- ^ String to split
         -> [String]  -- ^ Result
splitAll pattern = filter (not . null) . splitAll'
  where
    splitAll' src = case listToMaybe (src =~~ pattern) of
        Nothing     -> [src]
        Just (o, l) ->
            let (before, tmp) = splitAt o src
            in before : splitAll' (drop l tmp)

-- | Convert a filepath to an URL starting from the site root
--
-- Example:
--
-- > toUrl "foo/bar.html"
--
-- Result:
--
-- > "/foo/bar.html"
--
toUrl :: FilePath -> String
toUrl = ('/' :)

-- | Get the relative url to the site root, for a given (absolute) url
--
toSiteRoot :: String -> String
toSiteRoot = emptyException . joinPath . map parent . splitPath . takeDirectory
  where
    parent = const ".."
    emptyException [] = "."
    emptyException x  = x
