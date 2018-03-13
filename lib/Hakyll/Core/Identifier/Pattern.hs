--------------------------------------------------------------------------------
-- | As 'Identifier' is used to specify a single item, a 'Pattern' is used to
-- specify a list of items.
--
-- In most cases, globs are used for patterns.
--
-- A very simple pattern of such a pattern is @\"foo\/bar\"@. This pattern will
-- only match the exact @foo\/bar@ identifier.
--
-- To match more than one identifier, there are different captures that one can
-- use:
--
-- * @\"*\"@: matches at most one element of an identifier;
--
-- * @\"**\"@: matches one or more elements of an identifier.
--
-- Some examples:
--
-- * @\"foo\/*\"@ will match @\"foo\/bar\"@ and @\"foo\/foo\"@, but not
--   @\"foo\/bar\/qux\"@;
--
-- * @\"**\"@ will match any identifier;
--
-- * @\"foo\/**\"@ will match @\"foo\/bar\"@ and @\"foo\/bar\/qux\"@, but not
--   @\"bar\/foo\"@;
--
-- * @\"foo\/*.html\"@ will match all HTML files in the @\"foo\/\"@ directory.
--
-- The 'capture' function allows the user to get access to the elements captured
-- by the capture elements in a glob or regex pattern.
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hakyll.Core.Identifier.Pattern
    ( -- * The pattern type
      Pattern

      -- * Creating patterns
    , fromGlob
    , fromList
    , fromRegex
    , fromVersion
    , hasVersion
    , hasNoVersion

      -- * Composing patterns
    , (.&&.)
    , (.||.)
    , complement

      -- * Applying patterns
    , matches
    , filterMatches

      -- * Capturing strings
    , capture
    , fromCapture
    , fromCaptures
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                           ((&&&), (>>>))
import           Control.Monad                           (msum)
import           Data.List                               (inits, isPrefixOf,
                                                          tails)
import           Data.Maybe                              (isJust)
import qualified Data.Set                                as S


--------------------------------------------------------------------------------
import           GHC.Exts                                (IsString, fromString)
import           Text.Regex.TDFA                         ((=~))


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern.Internal


--------------------------------------------------------------------------------
instance IsString Pattern where
    fromString = fromGlob


--------------------------------------------------------------------------------
-- | Parse a pattern from a string
fromGlob :: String -> Pattern
fromGlob = Glob . parse'
  where
    parse' str =
        let (chunk, rest) = break (`elem` "\\*") str
        in case rest of
            ('\\' : x   : xs) -> Literal (chunk ++ [x]) : parse' xs
            ('*'  : '*' : xs) -> Literal chunk : CaptureMany : parse' xs
            ('*'  : xs)       -> Literal chunk : Capture : parse' xs
            xs                -> Literal chunk : Literal xs : []


--------------------------------------------------------------------------------
-- | Create a 'Pattern' from a list of 'Identifier's it should match.
--
-- /Warning/: use this carefully with 'hasNoVersion' and 'hasVersion'. The
-- 'Identifier's in the list /already/ have versions assigned, and the pattern
-- will then only match the intersection of both versions.
--
-- A more concrete example,
--
-- > fromList ["foo.markdown"] .&&. hasVersion "pdf"
--
-- will not match anything! The @"foo.markdown"@ 'Identifier' has no version
-- assigned, so the LHS of '.&&.' will only match this 'Identifier' with no
-- version. The RHS only matches 'Identifier's with version set to @"pdf"@ --
-- hence, this pattern matches nothing.
--
-- The correct way to use this is:
--
-- > fromList $ map (setVersion $ Just "pdf") ["foo.markdown"]
fromList :: [Identifier] -> Pattern
fromList = List . S.fromList


--------------------------------------------------------------------------------
-- | Create a 'Pattern' from a regex
--
-- Example:
--
-- > regex "^foo/[^x]*$
fromRegex :: String -> Pattern
fromRegex = Regex


--------------------------------------------------------------------------------
-- | Create a pattern which matches all items with the given version.
fromVersion :: Maybe String -> Pattern
fromVersion = Version


--------------------------------------------------------------------------------
-- | Specify a version, e.g.
--
-- > "foo/*.markdown" .&&. hasVersion "pdf"
hasVersion :: String -> Pattern
hasVersion = fromVersion . Just


--------------------------------------------------------------------------------
-- | Match only if the identifier has no version set, e.g.
--
-- > "foo/*.markdown" .&&. hasNoVersion
hasNoVersion :: Pattern
hasNoVersion = fromVersion Nothing


--------------------------------------------------------------------------------
-- | '&&' for patterns: the given identifier must match both subterms
(.&&.) :: Pattern -> Pattern -> Pattern
x .&&. y = And x y
infixr 3 .&&.


--------------------------------------------------------------------------------
-- | '||' for patterns: the given identifier must match any subterm
(.||.) :: Pattern -> Pattern -> Pattern
x .||. y = complement (complement x `And` complement y)  -- De Morgan's law
infixr 2 .||.


--------------------------------------------------------------------------------
-- | Inverts a pattern, e.g.
--
-- > complement "foo/bar.html"
--
-- will match /anything/ except @\"foo\/bar.html\"@
complement :: Pattern -> Pattern
complement = Complement


--------------------------------------------------------------------------------
-- | Check if an identifier matches a pattern
matches :: Pattern -> Identifier -> Bool
matches Everything     _ = True
matches (Complement p) i = not $ matches p i
matches (And x y)      i = matches x i && matches y i
matches (Glob p)       i = isJust $ capture (Glob p) i
matches (List l)       i = i `S.member` l
matches (Regex r)      i = toFilePath i =~ r
matches (Version v)    i = identifierVersion i == v


--------------------------------------------------------------------------------
-- | Given a list of identifiers, retain only those who match the given pattern
filterMatches :: Pattern -> [Identifier] -> [Identifier]
filterMatches = filter . matches


--------------------------------------------------------------------------------
-- | Split a list at every possible point, generate a list of (init, tail)
-- cases. The result is sorted with inits decreasing in length.
splits :: [a] -> [([a], [a])]
splits = inits &&& tails >>> uncurry zip >>> reverse


--------------------------------------------------------------------------------
-- | Match a glob or regex pattern against an identifier, generating a list of captures
capture :: Pattern -> Identifier -> Maybe [String]
capture (Glob p) i = capture' p (toFilePath i)
capture (Regex pat) i = Just groups
  where (_, _, _, groups) = ((toFilePath i) =~ pat) :: (String, String, String, [String])
capture _        _ = Nothing


--------------------------------------------------------------------------------
-- | Internal verion of 'capture'
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


--------------------------------------------------------------------------------
-- | Create an identifier from a pattern by filling in the captures with a given
-- string
--
-- Example:
--
-- > fromCapture (fromGlob "tags/*") "foo"
--
-- Result:
--
-- > "tags/foo"
fromCapture :: Pattern -> String -> Identifier
fromCapture pattern = fromCaptures pattern . repeat


--------------------------------------------------------------------------------
-- | Create an identifier from a pattern by filling in the captures with the
-- given list of strings
fromCaptures :: Pattern -> [String] -> Identifier
fromCaptures (Glob p) = fromFilePath . fromCaptures' p
fromCaptures _        = error $
    "Hakyll.Core.Identifier.Pattern.fromCaptures: fromCaptures only works " ++
    "on simple globs!"


--------------------------------------------------------------------------------
-- | Internally used version of 'fromCaptures'
fromCaptures' :: [GlobComponent] -> [String] -> String
fromCaptures' []        _ = mempty
fromCaptures' (m : ms) [] = case m of
    Literal l -> l `mappend` fromCaptures' ms []
    _         -> error $  "Hakyll.Core.Identifier.Pattern.fromCaptures': "
                       ++ "identifier list exhausted"
fromCaptures' (m : ms) ids@(i : is) = case m of
    Literal l -> l `mappend` fromCaptures' ms ids
    _         -> i `mappend` fromCaptures' ms is
