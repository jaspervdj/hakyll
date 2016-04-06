--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Core.Dependencies
    ( Dependency (..)
    , DependencyFacts
    , outOfDate
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                  (foldM, forM_, unless, when)
import           Control.Monad.Reader           (ask)
import           Control.Monad.RWS              (RWS, runRWS)
import qualified Control.Monad.State            as State
import           Control.Monad.Writer           (tell)
import           Data.Binary                    (Binary (..), getWord8,
                                                 putWord8)
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
data Dependency
    = PatternDependency Pattern (Set Identifier)
    | IdentifierDependency Identifier
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Binary Dependency where
    put (PatternDependency p is) = putWord8 0 >> put p >> put is
    put (IdentifierDependency i) = putWord8 1 >> put i
    get = getWord8 >>= \t -> case t of
        0 -> PatternDependency <$> get <*> get
        1 -> IdentifierDependency <$> get
        _ -> error "Data.Binary.get: Invalid Dependency"


--------------------------------------------------------------------------------
type DependencyFacts = Map Identifier [Dependency]


--------------------------------------------------------------------------------
outOfDate
    :: [Identifier]     -- ^ All known identifiers
    -> Set Identifier   -- ^ Initially out-of-date resources
    -> DependencyFacts  -- ^ Old dependency facts
    -> (Set Identifier, DependencyFacts, [String])
outOfDate universe ood oldFacts =
    let (_, state, logs) = runRWS rws universe (DependencyState oldFacts ood)
    in (dependencyOod state, dependencyFacts state, logs)
  where
    rws = do
        checkNew
        checkChangedPatterns
        bruteForce


--------------------------------------------------------------------------------
data DependencyState = DependencyState
    { dependencyFacts :: DependencyFacts
    , dependencyOod   :: Set Identifier
    } deriving (Show)


--------------------------------------------------------------------------------
type DependencyM a = RWS [Identifier] [String] DependencyState a


--------------------------------------------------------------------------------
markOod :: Identifier -> DependencyM ()
markOod id' = State.modify $ \s ->
    s {dependencyOod = S.insert id' $ dependencyOod s}


--------------------------------------------------------------------------------
dependenciesFor :: Identifier -> DependencyM [Identifier]
dependenciesFor id' = do
    facts <- dependencyFacts <$> State.get
    return $ concatMap dependenciesFor' $ fromMaybe [] $ M.lookup id' facts
  where
    dependenciesFor' (IdentifierDependency i) = [i]
    dependenciesFor' (PatternDependency _ is) = S.toList is


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
    go _   ds (IdentifierDependency i) = return $ IdentifierDependency i : ds
    go id' ds (PatternDependency p ls) = do
        universe <- ask
        let ls' = S.fromList $ filterMatches p universe
        if ls == ls'
            then return $ PatternDependency p ls : ds
            else do
                tell [show id' ++ " is out-of-date because a pattern changed"]
                markOod id'
                return $ PatternDependency p ls' : ds


--------------------------------------------------------------------------------
bruteForce :: DependencyM ()
bruteForce = do
    todo <- ask
    go todo
  where
    go todo = do
        (todo', changed) <- foldM check ([], False) todo
        when changed (go todo')

    check (todo, changed) id' = do
        deps <- dependenciesFor id'
        ood  <- dependencyOod <$> State.get
        case find (`S.member` ood) deps of
            Nothing -> return (id' : todo, changed)
            Just d  -> do
                tell [show id' ++ " is out-of-date because " ++
                    show d ++ " is out-of-date"]
                markOod id'
                return (todo, True)
