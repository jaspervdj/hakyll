--------------------------------------------------------------------------------
module Hakyll.Core.Dependencies
    ( Dependency (..)
    , DependencyFacts
    , outOfDate
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            ((<$>))
import           Control.Monad                  (foldM, forM_, unless, when)
import           Control.Monad.Reader           (ask)
import           Control.Monad.RWS              (RWS, runRWS)
import           Control.Monad.State            (get, modify)
import           Control.Monad.Writer           (tell)
import           Data.List                      (find)
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Maybe                     (fromMaybe)
import           Data.Set                       (Set)
import qualified Data.Set                       as S


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern


--------------------------------------------------------------------------------
data Dependency
    = Pattern (Pattern ()) [Identifier ()]
    | Identifier (Identifier ())
    deriving (Show)


--------------------------------------------------------------------------------
type DependencyFacts = Map (Identifier ()) [Dependency]


--------------------------------------------------------------------------------
outOfDate
    :: [Identifier ()]      -- ^ All known identifiers
    -> Set (Identifier ())  -- ^ Initially out-of-date resources
    -> DependencyFacts      -- ^ Old dependency facts
    -> (Set (Identifier ()), DependencyFacts, [String])
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
    , dependencyOod   :: Set (Identifier ())
    } deriving (Show)


--------------------------------------------------------------------------------
type DependencyM a = RWS [Identifier ()] [String] DependencyState a


--------------------------------------------------------------------------------
markOod :: Identifier () -> DependencyM ()
markOod id' = modify $ \s -> s {dependencyOod = S.insert id' $ dependencyOod s}


--------------------------------------------------------------------------------
dependenciesFor :: Identifier () -> DependencyM [Identifier ()]
dependenciesFor id' = do
    facts <- dependencyFacts <$> get
    let relevant = fromMaybe [] $ M.lookup id' facts
    return [i | Identifier i <- relevant]


--------------------------------------------------------------------------------
checkNew :: DependencyM ()
checkNew = do
    universe <- ask
    facts    <- dependencyFacts <$> get
    forM_ universe $ \id' -> unless (id' `M.member` facts) $ do
        tell [show id' ++ " is out-of-date because it is new"]
        markOod id'


--------------------------------------------------------------------------------
checkChangedPatterns :: DependencyM ()
checkChangedPatterns = do
    facts <- M.toList . dependencyFacts <$> get
    forM_ facts $ \(id', deps) -> do
        deps' <- foldM (go id') [] deps
        modify $ \s -> s
            {dependencyFacts = M.insert id' deps' $ dependencyFacts s}
  where
    go _   ds (Identifier i) = return $ Identifier i : ds
    go id' ds (Pattern p ls) = do
        universe <- ask
        let ls' = filterMatches p universe
        if ls == ls'
            then return $ Pattern p ls : ds
            else do
                tell [show id' ++ " is out-of-date because a pattern changed"]
                markOod id'
                return $ Pattern p ls' : ds


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
        ood  <- dependencyOod <$> get
        case find (`S.member` ood) deps of
            Nothing -> return (id' : todo, changed)
            Just d  -> do
                tell [show id' ++ " is out-of-date because " ++
                    show d ++ " is out-of-date"]
                markOod id'
                return (todo, True)
