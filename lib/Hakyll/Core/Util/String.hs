{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- | Miscellaneous string manipulation functions.
module Hakyll.Core.Util.String
    ( trim
    , replaceAll
    , splitAll
    , needlePrefix
    ) where


--------------------------------------------------------------------------------
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Text.Regex.TDFA ((=~~))


--------------------------------------------------------------------------------
-- | Trim a string (drop spaces, tabs and newlines at both sides).
trim :: String -> String
trim = reverse . trim' . reverse . trim'
  where
    trim' = dropWhile isSpace


--------------------------------------------------------------------------------
-- | A simple (but inefficient) regex replace funcion
replaceAll :: String              -- ^ Pattern
           -> (String -> String)  -- ^ Replacement (called on match)
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


--------------------------------------------------------------------------------
-- | A simple regex split function. The resulting list will contain no empty
-- strings.
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



--------------------------------------------------------------------------------
-- | Find the first instance of needle (must be non-empty) in haystack. We
-- return the prefix of haystack before needle is matched.
--
-- Examples:
--
-- > needlePrefix "cd" "abcde" = "ab"
--
-- > needlePrefix "ab" "abc" = ""
--
-- > needlePrefix "ab" "xxab" = "xx"
--
-- > needlePrefix "a" "xx" = "xx"
needlePrefix :: String -> String -> Maybe String
needlePrefix needle haystack = go [] haystack
  where
    go _   []                     = Nothing
    go acc xss@(x:xs)
        | needle `isPrefixOf` xss = Just $ reverse acc
        | otherwise               = go (x : acc) xs
