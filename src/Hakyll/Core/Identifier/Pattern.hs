-- | Module providing pattern matching and capturing on 'Identifier's.
--
-- A very simple pattern could be, for example, @foo\/bar@. This pattern will
-- only match the exact @foo\/bar@ identifier.
--
-- To match more than one identifier, there are different captures that one can
-- use:
--
-- * @*@: matches at most one element of an identifier;
--
-- * @**@: matches one or more elements of an identifier.
--
-- Some examples:
--
-- * @foo\/*@ will match @foo\/bar@ and @foo\/foo@, but not @foo\/bar\/qux@;
--
-- * @**@ will match any identifier;
--
-- * @foo\/**@ will match @foo\/bar@ and @foo\/bar\/qux@, but not @bar\/foo@;
--
-- * @foo\/*.html@ will match all HTML files in the @foo\/@ directory.
--
-- The 'match' function allows the user to get access to the elements captured
-- by the capture elements in the pattern.
--
module Hakyll.Core.Identifier.Pattern
    ( Pattern
    , parsePattern
    , match
    , doesMatch
    , matches
    , fromCapture
    , fromCaptureString
    , fromCaptures
    ) where

import Data.List (isPrefixOf, inits, tails)
import Control.Arrow ((&&&), (>>>))
import Control.Monad (msum)
import Data.Maybe (isJust)
import Data.Monoid (mempty, mappend)

import GHC.Exts (IsString, fromString)

import Hakyll.Core.Identifier

-- | One base element of a pattern
--
data PatternComponent = Capture
                      | CaptureMany
                      | Literal String
                      deriving (Eq, Show)

-- | Type that allows matching on identifiers
--
newtype Pattern = Pattern {unPattern :: [PatternComponent]}
                deriving (Eq, Show)

instance IsString Pattern where
    fromString = parsePattern

-- | Parse a pattern from a string
--
parsePattern :: String -> Pattern
parsePattern = Pattern . parse' -- undefined -- Pattern . map toPattern . unIdentifier . parseIdentifier
  where
    parse' str =
        let (chunk, rest) = break (`elem` "\\*") str
        in case rest of
            ('\\' : x   : xs) -> Literal (chunk ++ [x]) : parse' xs
            ('*'  : '*' : xs) -> Literal chunk : CaptureMany : parse' xs
            ('*'  : xs)       -> Literal chunk : Capture : parse' xs
            xs                -> Literal chunk : Literal xs : []

-- | Match an identifier against a pattern, generating a list of captures
--
match :: Pattern -> Identifier -> Maybe [Identifier]
match p (Identifier i) = fmap (map Identifier) $ match' (unPattern p) i

-- | Check if an identifier matches a pattern
--
doesMatch :: Pattern -> Identifier -> Bool
doesMatch p = isJust . match p

-- | Given a list of identifiers, retain only those who match the given pattern
--
matches :: Pattern -> [Identifier] -> [Identifier]
matches p = filter (doesMatch p)

-- | Split a list at every possible point, generate a list of (init, tail)
-- cases. The result is sorted with inits decreasing in length.
--
splits :: [a] -> [([a], [a])]
splits = inits &&& tails >>> uncurry zip >>> reverse

-- | Internal verion of 'match'
--
match' :: [PatternComponent] -> String -> Maybe [String]
match' [] [] = Just []  -- An empty match
match' [] _  = Nothing  -- No match
-- match' _  [] = Nothing   -- No match
match' (Literal l : ms) str
    -- Match the literal against the string
    | l `isPrefixOf` str = match' ms $ drop (length l) str
    | otherwise          = Nothing
match' (Capture : ms) str =
    -- Match until the next /
    let (chunk, rest) = break (== '/') str
    in msum $ [ fmap (i :) (match' ms (t ++ rest)) | (i, t) <- splits chunk ]
match' (CaptureMany : ms) str =
    -- Match everything
    msum $ [ fmap (i :) (match' ms t) | (i, t) <- splits str ]
    
-- | Create an identifier from a pattern by filling in the captures with a given
-- string
--
-- Example:
--
-- > fromCapture (parsePattern "tags/*") (parseIdentifier "foo")
--
-- Result:
--
-- > "tags/foo"
--
fromCapture :: Pattern -> Identifier -> Identifier
fromCapture pattern = fromCaptures pattern . repeat

-- | Simplified version of 'fromCapture' which takes a 'String' instead of an
-- 'Identifier'
--
-- > fromCaptureString (parsePattern "tags/*") "foo"
--
-- Result:
--
-- > "tags/foo"
--
fromCaptureString :: Pattern -> String -> Identifier
fromCaptureString pattern = fromCapture pattern . parseIdentifier

-- | Create an identifier from a pattern by filling in the captures with the
-- given list of strings
--
fromCaptures :: Pattern -> [Identifier] -> Identifier
fromCaptures (Pattern []) _ = mempty
fromCaptures (Pattern (m : ms)) [] = case m of
    Literal l -> Identifier l `mappend` fromCaptures (Pattern ms) []
    _         -> error $  "Hakyll.Core.Identifier.Pattern.fromCaptures: "
                       ++ "identifier list exhausted"
fromCaptures (Pattern (m : ms)) ids@(i : is) = case m of
    Literal l -> Identifier l `mappend` fromCaptures (Pattern ms) ids
    _         -> i `mappend` fromCaptures (Pattern ms) is
