-- | This internal module is mostly here to prevent CPP conflicting with Haskell
-- comments.
{-# LANGUAGE CPP #-}
module Hakyll.Core.Identifier.Pattern.Internal
    ( GlobComponent (..)
    , Pattern (..)
    ) where


--------------------------------------------------------------------------------
import           Data.Binary            (Binary (..), getWord8, putWord8)
import           Data.Set               (Set)


--------------------------------------------------------------------------------
#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup         (Semigroup (..))
#endif


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
    put (Glob g)       = putWord8 3 >> put g
    put (List is)      = putWord8 4 >> put is
    put (Regex r)      = putWord8 5 >> put r
    put (Version v)    = putWord8 6 >> put v

    get = getWord8 >>= \t -> case t of
        0 -> pure Everything
        1 -> Complement <$> get
        2 -> And <$> get <*> get
        3 -> Glob <$> get
        4 -> List <$> get
        5 -> Regex <$> get
        _ -> Version <$> get


--------------------------------------------------------------------------------
#if MIN_VERSION_base(4,9,0)
instance Semigroup Pattern where
    (<>) = And

instance Monoid Pattern where
    mempty  = Everything
    mappend = (<>)
#else
instance Monoid Pattern where
    mempty  = Everything
    mappend = And
#endif
