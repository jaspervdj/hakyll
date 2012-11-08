--------------------------------------------------------------------------------
-- | Module providing pattern matching and capturing on file names.
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
module Hakyll.Core.Resource.Pattern
    ( Pattern
    , parsePattern
    , capture
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                ((&&&), (>>>))
import           Control.Monad                (msum)
import           Data.List                    (inits, isPrefixOf, tails)
import           GHC.Exts                     (IsString, fromString)


--------------------------------------------------------------------------------
import           Hakyll.Core.Resource


--------------------------------------------------------------------------------
-- | One base element of a pattern
data GlobComponent
    = Capture
    | CaptureMany
    | Literal String
    deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | Type that allows matching on identifiers
newtype Pattern = Pattern [GlobComponent]
    deriving (Show)


--------------------------------------------------------------------------------
instance IsString Pattern where
    fromString = parsePattern


--------------------------------------------------------------------------------
-- | Parse a pattern from a string
parsePattern :: String -> Pattern
parsePattern = Pattern . parse
  where
    parse str =
        let (chunk, rest) = break (`elem` "\\*") str
        in case rest of
            ('\\' : x   : xs) -> Literal (chunk ++ [x]) : parse xs
            ('*'  : '*' : xs) -> Literal chunk : CaptureMany : parse xs
            ('*'  : xs)       -> Literal chunk : Capture : parse xs
            xs                -> Literal chunk : Literal xs : []


--------------------------------------------------------------------------------
-- | Split a list at every possible point, generate a list of (init, tail)
-- cases. The result is sorted with inits decreasing in length.
splits :: [a] -> [([a], [a])]
splits = inits &&& tails >>> uncurry zip >>> reverse


--------------------------------------------------------------------------------
-- | Match a glob against a pattern, generating a list of captures
capture :: Pattern -> Resource -> Maybe [String]
capture (Pattern p) rs = capture' p (unResource rs)


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
-- > fromCapture (parsePattern "tags/*") "foo"
--
-- Result:
--
-- > "tags/foo"
{-
fromCapture :: Pattern -> String -> Identifier
fromCapture pattern = fromCaptures pattern . repeat
-}


--------------------------------------------------------------------------------
-- | Create an identifier from a pattern by filling in the captures with the
-- given list of strings
--
{-
fromCaptures :: Pattern -> [String] -> String
fromCaptures (Pattern p) = fromCaptures' p
-}


--------------------------------------------------------------------------------
-- | Internally used version of 'fromCaptures'
{-
fromCaptures' :: [GlobComponent] -> [String] -> String
fromCaptures' []        _ = mempty
fromCaptures' (m : ms) [] = case m of
    Literal l -> l `mappend` fromCaptures' ms []
    _         -> error $  "Hakyll.Core.Identifier.Pattern.fromCaptures': "
                       ++ "identifier list exhausted"
fromCaptures' (m : ms) ids@(i : is) = case m of
    Literal l -> l `mappend` fromCaptures' ms ids
    _         -> i `mappend` fromCaptures' ms is
-}
