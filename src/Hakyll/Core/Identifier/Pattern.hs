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
-- by the capture elements in the pattern.
module Hakyll.Core.Identifier.Pattern
    ( -- * The pattern type
      Pattern

      -- * Creating patterns
    , fromGlob
    , fromList
    , fromRegex
    , fromVersion

      -- * Manipulating patterns
    , (.&&.)
    , (.||.)
    , complement
    , withVersion
    , fromLiteral

      -- * Applying patterns
    , matches
    , filterMatches

      -- * Capturing strings
    , capture
    , fromCapture
    , fromCaptures
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    (pure, (<$>), (<*>))
import           Control.Arrow          ((&&&), (>>>))
import           Control.Monad          (msum)
import           Data.Binary            (Binary (..), getWord8, putWord8)
import           Data.List              (inits, isPrefixOf, tails)
import           Data.Maybe             (isJust)
import           Data.Monoid            (Monoid, mappend, mempty)
import           Data.Set               (Set)
import qualified Data.Set               as S


--------------------------------------------------------------------------------
import           GHC.Exts               (IsString, fromString)
import           Text.Regex.TDFA        ((=~))


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier


--------------------------------------------------------------------------------
-- | Elements of a glob pattern
data GlobComponent
    = Capture
    | CaptureMany
    | Literal String
    deriving (Eq, Show)


--------------------------------------------------------------------------------
instance Binary GlobComponent where
    put Capture     = putWord8 0
    put CaptureMany = putWord8 1
    put (Literal s) = putWord8 2 >> put s

    get = getWord8 >>= \t -> case t of
        0 -> pure Capture
        1 -> pure CaptureMany
        2 -> Literal <$> get
        _ -> error "Data.Binary.get: Invalid GlobComponent"


--------------------------------------------------------------------------------
-- | Type that allows matching on identifiers
data Pattern
    = Everything
    | Complement Pattern
    | And Pattern Pattern
    | Or Pattern Pattern
    | Glob [GlobComponent]
    | List (Set Identifier)
    | Regex String
    | Version (Maybe String)
    deriving (Show)


--------------------------------------------------------------------------------
instance Binary Pattern where
    put Everything     = putWord8 0
    put (Complement p) = putWord8 1 >> put p
    put (And x y)      = putWord8 2 >> put x >> put y
    put (Or x y)       = putWord8 3 >> put x >> put y
    put (Glob g)       = putWord8 4 >> put g
    put (List is)      = putWord8 5 >> put is
    put (Regex r)      = putWord8 6 >> put r
    put (Version v)    = putWord8 7 >> put v

    get = getWord8 >>= \t -> case t of
        0 -> pure Everything
        1 -> Complement <$> get
        2 -> And <$> get <*> get
        3 -> Or <$> get <*> get
        4 -> Glob <$> get
        5 -> List <$> get
        6 -> Regex <$> get
        _ -> Version <$> get


--------------------------------------------------------------------------------
instance IsString Pattern where
    fromString = fromGlob


--------------------------------------------------------------------------------
instance Monoid Pattern where
    mempty  = Everything
    mappend = (.&&.)


--------------------------------------------------------------------------------
-- | This is necessary for good 'isLiteral' results
optimize :: Pattern -> Pattern
optimize (Complement x)     = Complement (optimize x)
optimize (And x Everything) = x
optimize (And Everything y) = y
optimize (And x y)          = And (optimize x) (optimize y)
optimize (Or _ Everything)  = Everything
optimize (Or Everything _)  = Everything
optimize (Or x y)           = Or (optimize x) (optimize y)
optimize p                  = p


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
-- | Create a 'Pattern' from a list of 'Identifier's it should match
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
-- | '&&' for patterns: the given identifier must match both subterms
(.&&.) :: Pattern -> Pattern -> Pattern
x .&&. y = optimize (And x y)
infixr 3 .&&.


--------------------------------------------------------------------------------
-- | '||' for patterns: the given identifier must match any subterm
(.||.) :: Pattern -> Pattern -> Pattern
x .||. y = optimize (Or x y)
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
-- | Specify a version, e.g.
--
-- > "foo/*.markdown" `withVersion` "pdf"
withVersion :: Pattern -> String -> Pattern
withVersion p v = optimize $ And p $ fromVersion $ Just v


--------------------------------------------------------------------------------
-- | Check if a pattern is a literal. @\"*.markdown\"@ is not a literal but
-- @\"posts.markdown\"@ is.
fromLiteral :: Pattern -> Maybe Identifier
fromLiteral pattern = case pattern of
    Glob p -> fmap fromFilePath $ foldr fromLiteral' (Just "") p
    _      -> Nothing
  where
    fromLiteral' (Literal x) (Just y) = Just $ x ++ y
    fromLiteral' _           _        = Nothing



--------------------------------------------------------------------------------
-- | Check if an identifier matches a pattern
matches :: Pattern -> Identifier -> Bool
matches Everything     _ = True
matches (Complement p) i = not $ matches p i
matches (And x y)      i = matches x i && matches y i
matches (Or x y)       i = matches x i || matches y i
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
-- | Match a glob against a pattern, generating a list of captures
capture :: Pattern -> Identifier -> Maybe [String]
capture (Glob p) i = capture' p (toFilePath i)
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
-- > fromCapture (parseGlob "tags/*") "foo"
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
