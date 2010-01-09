-- | A module that exports a simple regex interface. This code is mostly copied
--   from the regex-compat package at hackage.
module Text.Hakyll.Regex
    ( split
    , substitute
    ) where

import Text.Regex.TDFA

-- | Match a regular expression against a string, returning more information
--   about the match.
matchRegexAll :: Regex -> String -> Maybe (String, String, String, [String])
matchRegexAll p str = matchM p str

-- | Replaces every occurance of the given regexp with the replacement string.
subRegex :: Regex                          -- ^ Search pattern
      -> String                         -- ^ Input string
      -> String                         -- ^ Replacement text
      -> String                         -- ^ Output string
subRegex _ "" _ = ""
subRegex regexp inp replacement =
  let -- bre matches a backslash then capture either a backslash or some digits
      bre = makeRegex "\\\\(\\\\|[0-9]+)"
      lookup' _ [] _ = []
      lookup' [] _ _ = []
      lookup' match' repl groups =
        case matchRegexAll bre repl of
          Nothing -> repl
          Just (lead, _, trail, bgroups) ->
            let newval =
                 if (head bgroups) == "\\"
                   then "\\"
                   else let index :: Int
                            index = (read (head bgroups)) - 1
                        in if index == -1
                             then match'
                             else groups !! index
            in lead ++ newval ++ lookup' match' trail groups
  in case matchRegexAll regexp inp of
       Nothing -> inp
       Just (lead, match', trail, groups) ->
         lead ++ lookup' match' replacement groups ++ (subRegex regexp trail replacement)

-- | Splits a string based on a regular expression.  The regular expression
--   should identify one delimiter.
splitRegex :: Regex -> String -> [String]
splitRegex _ [] = []
splitRegex delim strIn = loop strIn where
  loop str = case matchOnceText delim str of
                Nothing -> [str]
                Just (firstline, _, remainder) ->
                  if null remainder
                    then [firstline,""]
                    else firstline : loop remainder

-- | Split a list at a certain element.
split :: String -> String -> [String]
split pattern = filter (not . null)
              . splitRegex (makeRegex pattern)

-- | Substitute a regex. Simplified interface.
substitute :: String -- ^ Pattern to replace (regex).
           -> String -- ^ Replacement string.
           -> String -- ^ Input string.
           -> String -- ^ Result.
substitute pattern replacement str = subRegex (makeRegex pattern) str replacement
