-- | Module providing pattern matching and capturing on 'Identifier's.
-- 'Pattern's come in two kinds:
--
-- * Simple glob patterns, like @foo\/*@;
--
-- * Custom, arbitrary predicates of the type @Identifier -> Bool@.
--
-- They both have advantages and disadvantages. By default, globs are used,
-- unless you construct your 'Pattern' using the 'predicate' function.
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
-- The 'capture' function allows the user to get access to the elements captured
-- by the capture elements in the pattern.
--
module Hakyll.Core.Identifier.Pattern
    ( Pattern
    , parseGlob
    , predicate
    , matches
    , filterMatches
    , capture
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
data GlobComponent = Capture
                   | CaptureMany
                   | Literal String
                   deriving (Eq, Show)

-- | Type that allows matching on identifiers
--
data Pattern = Glob [GlobComponent]
             | Predicate (Identifier -> Bool)

instance IsString Pattern where
    fromString = parseGlob

-- | Parse a pattern from a string
--
parseGlob :: String -> Pattern
parseGlob = Glob . parse'
  where
    parse' str =
        let (chunk, rest) = break (`elem` "\\*") str
        in case rest of
            ('\\' : x   : xs) -> Literal (chunk ++ [x]) : parse' xs
            ('*'  : '*' : xs) -> Literal chunk : CaptureMany : parse' xs
            ('*'  : xs)       -> Literal chunk : Capture : parse' xs
            xs                -> Literal chunk : Literal xs : []

-- | Create a 'Pattern' from an arbitrary predicate
--
-- Example:
--
-- > predicate (\i -> matches "foo/*" i && not (matches "foo/bar" i))
--
predicate :: (Identifier -> Bool) -> Pattern
predicate = Predicate

-- | Check if an identifier matches a pattern
--
matches :: Pattern -> Identifier -> Bool
matches (Glob p)      = isJust . capture (Glob p)
matches (Predicate p) = (p $)

-- | Given a list of identifiers, retain only those who match the given pattern
--
filterMatches :: Pattern -> [Identifier] -> [Identifier]
filterMatches = filter . matches

-- | Split a list at every possible point, generate a list of (init, tail)
-- cases. The result is sorted with inits decreasing in length.
--
splits :: [a] -> [([a], [a])]
splits = inits &&& tails >>> uncurry zip >>> reverse

-- | Match a glob against a pattern, generating a list of captures
--
capture :: Pattern -> Identifier -> Maybe [Identifier]
capture (Glob p) (Identifier i) = fmap (map Identifier) $ capture' p i
capture (Predicate _) _         = Nothing

-- | Internal verion of 'capture'
--
capture' :: [GlobComponent] -> String -> Maybe [String]
capture' [] [] = Just []  -- An empty match
capture' [] _  = Nothing  -- No match
capture' (Literal l : ms) str
    -- Match the literal against the string
    | l `isPrefixOf` str = capture' ms $ drop (length l) str
    | otherwise          = Nothing
capture' (Capture : ms) str =
    -- Match until the next /
    let (chunk, rest) = break (== '/') str
    in msum $ [ fmap (i :) (capture' ms (t ++ rest)) | (i, t) <- splits chunk ]
capture' (CaptureMany : ms) str =
    -- Match everything
    msum $ [ fmap (i :) (capture' ms t) | (i, t) <- splits str ]
    
-- | Create an identifier from a pattern by filling in the captures with a given
-- string
--
-- Example:
--
-- > fromCapture (parseGlob "tags/*") (parseIdentifier "foo")
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
-- > fromCaptureString (parseGlob "tags/*") "foo"
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
fromCaptures (Glob p)      = fromCaptures' p
fromCaptures (Predicate _) = error $
    "Hakyll.Core.Identifier.Pattern.fromCaptures: fromCaptures called on a " ++
    "predicate instead of a glob"

-- | Internally used version of 'fromCaptures'
--
fromCaptures' :: [GlobComponent] -> [Identifier] -> Identifier
fromCaptures' []        _ = mempty
fromCaptures' (m : ms) [] = case m of
    Literal l -> Identifier l `mappend` fromCaptures' ms []
    _         -> error $  "Hakyll.Core.Identifier.Pattern.fromCaptures': "
                       ++ "identifier list exhausted"
fromCaptures' (m : ms) ids@(i : is) = case m of
    Literal l -> Identifier l `mappend` fromCaptures' ms ids
    _         -> i `mappend` fromCaptures' ms is
