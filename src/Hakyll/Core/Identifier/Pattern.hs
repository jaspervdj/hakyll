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
-- Like an 'Identifier', a 'Pattern' also has a type parameter. This is simply
-- an extra layer of safety, and can be discarded using the 'castPattern'
-- function.
--
module Hakyll.Core.Identifier.Pattern
    ( -- * The pattern type
      Pattern
    , castPattern

      -- * Creating patterns
    , parseGlob
    , predicate
    , list
    , regex
    , inGroup
    , complement

      -- * Applying patterns
    , matches
    , filterMatches
    , capture
    , fromCapture
    , fromCaptures
    ) where

import Data.List (isPrefixOf, inits, tails)
import Control.Arrow ((&&&), (>>>))
import Control.Monad (msum)
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (Monoid, mempty, mappend)

import GHC.Exts (IsString, fromString)
import Text.Regex.TDFA ((=~~))

import Hakyll.Core.Identifier

-- | One base element of a pattern
--
data GlobComponent = Capture
                   | CaptureMany
                   | Literal String
                   deriving (Eq, Show)

-- | Type that allows matching on identifiers
--
data Pattern a = Glob [GlobComponent]
               | Predicate (Identifier a -> Bool)
               | List [Identifier a]

instance IsString (Pattern a) where
    fromString = parseGlob

instance Monoid (Pattern a) where
    mempty = Predicate (const True)
    p1 `mappend` p2 = Predicate $ \i -> matches p1 i && matches p2 i

-- | Discard the phantom type parameter
--
castPattern :: Pattern a -> Pattern b
castPattern (Glob g)      = Glob g
castPattern (Predicate p) = Predicate $ p . castIdentifier
castPattern (List l)      = List $ map castIdentifier l
{-# INLINE castPattern #-}

-- | Parse a pattern from a string
--
parseGlob :: String -> Pattern a
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
predicate :: (Identifier a -> Bool) -> Pattern a
predicate = Predicate

-- | Create a 'Pattern' from a list of 'Identifier's it should match
--
list :: [Identifier a] -> Pattern a
list = List

-- | Create a 'Pattern' from a regex
--
-- Example:
--
-- > regex "^foo/[^x]*$
--
regex :: String -> Pattern a
regex str = predicate $ fromMaybe False . (=~~ str) . toFilePath

-- | Create a 'Pattern' which matches if the identifier is in a certain group
-- (or in no group)
--
inGroup :: Maybe String -> Pattern a
inGroup group = predicate $ (== group) . identifierGroup

-- | Inverts a pattern, e.g.
--
-- > complement "foo/bar.html"
--
-- will match /anything/ except @\"foo\/bar.html\"@
--
complement :: Pattern a -> Pattern a
complement p = predicate (not . matches p)

-- | Check if an identifier matches a pattern
--
matches :: Pattern a -> Identifier a -> Bool
matches (Glob p)      = isJust . capture (Glob p)
matches (Predicate p) = (p $)
matches (List l)      = (`elem` l)

-- | Given a list of identifiers, retain only those who match the given pattern
--
filterMatches :: Pattern a -> [Identifier a] -> [Identifier a]
filterMatches = filter . matches

-- | Split a list at every possible point, generate a list of (init, tail)
-- cases. The result is sorted with inits decreasing in length.
--
splits :: [a] -> [([a], [a])]
splits = inits &&& tails >>> uncurry zip >>> reverse

-- | Match a glob against a pattern, generating a list of captures
--
capture :: Pattern a -> Identifier a -> Maybe [String]
capture (Glob p) (Identifier _ i) = capture' p i
capture _        _                = Nothing

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
-- > fromCapture (parseGlob "tags/*") "foo"
--
-- Result:
--
-- > "tags/foo"
--
fromCapture :: Pattern a -> String -> Identifier a
fromCapture pattern = fromCaptures pattern . repeat

-- | Create an identifier from a pattern by filling in the captures with the
-- given list of strings
--
fromCaptures :: Pattern a -> [String] -> Identifier a
fromCaptures (Glob p) = Identifier Nothing . fromCaptures' p
fromCaptures _        = error $
    "Hakyll.Core.Identifier.Pattern.fromCaptures: fromCaptures only works " ++
    "on simple globs!"

-- | Internally used version of 'fromCaptures'
--
fromCaptures' :: [GlobComponent] -> [String] -> String
fromCaptures' []        _ = mempty
fromCaptures' (m : ms) [] = case m of
    Literal l -> l `mappend` fromCaptures' ms []
    _         -> error $  "Hakyll.Core.Identifier.Pattern.fromCaptures': "
                       ++ "identifier list exhausted"
fromCaptures' (m : ms) ids@(i : is) = case m of
    Literal l -> l `mappend` fromCaptures' ms ids
    _         -> i `mappend` fromCaptures' ms is
