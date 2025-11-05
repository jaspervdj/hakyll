--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
module Hakyll.Core.Dependencies
    ( Dependency (..)
    , DependencySelector (..)
    , DependencyKind (..)
    , DependencyFacts
    , outOfDate
    , contentDependency
    , metadataDependency
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                  (foldM, forM_, unless, when)
import           Control.Monad.Reader           (ask)
import           Control.Monad.RWS              (RWS, runRWS)
import qualified Control.Monad.State            as State
import           Control.Monad.Writer           (tell)
import           Data.Binary                    (Binary (..), getWord8,
                                                 putWord8)
import           Data.Functor                   ((<&>))
import           Data.List                      (find)
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Maybe                     (fromMaybe)
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import           Data.Typeable                  (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern


--------------------------------------------------------------------------------
data DependencySelector
    = PatternDependency Pattern (Set Identifier)
    | IdentifierDependency Identifier
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Binary DependencySelector where
    put (PatternDependency p is) = putWord8 0 >> put p >> put is
    put (IdentifierDependency i) = putWord8 1 >> put i
    get = getWord8 >>= \t -> case t of
        0 -> PatternDependency <$> get <*> get
        1 -> IdentifierDependency <$> get
        _ -> fail "Data.Binary.get: Invalid DependencySelector"


--------------------------------------------------------------------------------
-- | A data type representing a dependency on another 'Identifier'. We can
-- depend either on the 'Hakyll.Core.Metadata.Metadata' or the entire content of
-- the underlying file. This is signified by the supplied 'DependencyKind'.
data Dependency
    = Dependency DependencyKind DependencySelector
    | AlwaysOutOfDate
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
-- | Utility function to create a new content dependency.
contentDependency :: DependencySelector -> Dependency
contentDependency = Dependency KindContent

-- | Utility function to a create a new metadata dependency.
metadataDependency :: DependencySelector -> Dependency
metadataDependency = Dependency KindMetadata


--------------------------------------------------------------------------------
instance Binary Dependency where
    put AlwaysOutOfDate = putWord8 2
    put (Dependency k s) = putWord8 3 >> put k >> put s

    get = getWord8 >>= \t -> case t of
        -- XXX: Backwards compatability with Hakyll <=4.16.7.1.
        0 -> (\p i -> contentDependency $ PatternDependency p i) <$> get <*> get
        1 -> contentDependency . IdentifierDependency <$> get

        2 -> pure AlwaysOutOfDate
        3 -> Dependency <$> get <*> get
        _ -> fail "Data.Binary.get: Invalid Dependency"


--------------------------------------------------------------------------------
type DependencyFacts = Map Identifier [Dependency]


--------------------------------------------------------------------------------
outOfDate
    :: [Identifier]     -- ^ All known identifiers
    -> Set Identifier   -- ^ Changed content
    -> Set Identifier   -- ^ Changed metadata
    -> DependencyFacts  -- ^ Old dependency facts
    -> (Set Identifier, DependencyFacts, [String])
outOfDate universe ood oodMeta oldFacts =
    let (_, state, logs) = runRWS rws universe (DependencyState oldFacts ood oodMeta)
    in (dependencyOod state, dependencyFacts state, logs)
  where
    rws = do
        checkNew
        checkChangedPatterns
        bruteForce


--------------------------------------------------------------------------------
data DependencyState = DependencyState
    { dependencyFacts   :: DependencyFacts
    , dependencyOod     :: Set Identifier
    , dependencyOodMeta :: Set Identifier
    } deriving (Show)


--------------------------------------------------------------------------------
type DependencyM a = RWS [Identifier] [String] DependencyState a


--------------------------------------------------------------------------------
markOod :: Identifier -> DependencyM ()
markOod id' = State.modify $ \s ->
    s {dependencyOod = S.insert id' $ dependencyOod s}


--------------------------------------------------------------------------------
data DependencyKind = KindContent | KindMetadata
  deriving (Show)

instance Binary DependencyKind where
  put KindContent = putWord8 0
  put KindMetadata = putWord8 1

  get = getWord8 >>= \t -> case t of
      0 -> pure KindContent
      1 -> pure KindMetadata
      _ -> fail "Data.Binary.get: Invalid DependencyKind"

--------------------------------------------------------------------------------
-- | Collection of dependencies that should be checked to determine
-- if an identifier needs rebuilding.
data Dependencies
  = DependsOn [(DependencyKind, Identifier)]
  | MustRebuild
  deriving (Show)

instance Semigroup Dependencies where
  DependsOn ids <> DependsOn moreIds = DependsOn (ids <> moreIds)
  MustRebuild <> _ = MustRebuild
  _ <> MustRebuild = MustRebuild

instance Monoid Dependencies where
  mempty = DependsOn []

--------------------------------------------------------------------------------
dependenciesFor :: Identifier -> DependencyM Dependencies
dependenciesFor id' = do
    facts <- dependencyFacts <$> State.get
    return $ foldMap dependenciesFor' $ fromMaybe [] $ M.lookup id' facts
  where
    dependenciesForSelector (IdentifierDependency i) = [i]
    dependenciesForSelector (PatternDependency _ is) = S.toList is

    dependenciesFor' AlwaysOutOfDate            = MustRebuild
    dependenciesFor' (Dependency kind selector) = DependsOn $
        map (kind,) $ dependenciesForSelector selector


--------------------------------------------------------------------------------
checkNew :: DependencyM ()
checkNew = do
    universe <- ask
    facts    <- dependencyFacts <$> State.get
    forM_ universe $ \id' -> unless (id' `M.member` facts) $ do
        tell [show id' ++ " is out-of-date because it is new"]
        markOod id'


--------------------------------------------------------------------------------
checkChangedPatterns :: DependencyM ()
checkChangedPatterns = do
    facts <- M.toList . dependencyFacts <$> State.get
    forM_ facts $ \(id', deps) -> do
        deps' <- foldM (go id') [] deps
        State.modify $ \s -> s
            {dependencyFacts = M.insert id' deps' $ dependencyFacts s}
  where
    go' _   (IdentifierDependency i) = return $ IdentifierDependency i
    go' id' (PatternDependency p ls) = do
        universe <- ask
        let ls' = S.fromList $ filterMatches p universe
        if ls == ls'
            then return $ PatternDependency p ls
            else do
                tell [show id' ++ " is out-of-date because a pattern changed"]
                markOod id'
                return $ PatternDependency p ls'

    go _   ds AlwaysOutOfDate          = return $ AlwaysOutOfDate : ds
    go id' ds (Dependency kind select) = (Dependency kind <$> go' id' select) <&> (: ds)


--------------------------------------------------------------------------------
bruteForce :: DependencyM ()
bruteForce = do
    todo <- ask
    go todo
  where
    go todo = do
        (todo', changed) <- foldM check ([], False) todo
        when changed (go todo')

    findOod oodContent oodMetadata (k, i)
      = S.member i $
          case k of
            KindContent  -> oodContent
            KindMetadata -> oodMetadata

    check (todo, changed) id' = do
        deps <- dependenciesFor id'
        case deps of
          DependsOn depList -> do
            ood     <- dependencyOod <$> State.get
            oodMeta <- dependencyOodMeta <$> State.get
            case find (findOod ood oodMeta) depList of
                Nothing -> return (id' : todo, changed)
                Just d  -> do
                    tell [show id' ++ " is out-of-date because " ++
                        show d ++ " is out-of-date"]
                    markOod id'
                    return (todo, True)
          MustRebuild -> do
            tell [show id' ++ " will be forcibly rebuilt"]
            markOod id'
            return (todo, True)
