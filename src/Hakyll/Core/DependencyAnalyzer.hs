--------------------------------------------------------------------------------
module Hakyll.Core.DependencyAnalyzer
    ( Analysis (..)
    , analyze
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.DeepSeq           (NFData (..))
import           Control.Monad             (filterM, forM_, msum, when)
import           Control.Monad.Reader      (ask)
import           Control.Monad.RWS         (RWS, runRWS)
import           Control.Monad.State       (evalState, get, modify)
import           Control.Monad.Writer      (tell)
import qualified Data.Map                  as M
import           Data.Set                  (Set)
import qualified Data.Set                  as S


--------------------------------------------------------------------------------
import           Hakyll.Core.DirectedGraph


--------------------------------------------------------------------------------
data Analysis a
    = Cycle [a]
    | Order [a]
    deriving (Show)


--------------------------------------------------------------------------------
instance NFData a => NFData (Analysis a) where
    rnf (Cycle c) = rnf c `seq` ()
    rnf (Order o) = rnf o `seq` ()


--------------------------------------------------------------------------------
analyze :: Ord a
        => DirectedGraph a  -- ^ Old graph
        -> DirectedGraph a  -- ^ New graph
        -> (a -> Bool)      -- ^ Out of date?
        -> Analysis a       -- ^ Result
analyze old new ood = case findCycle new of
    Just c  -> Cycle c
    Nothing -> Order $ findOrder old new ood


--------------------------------------------------------------------------------
-- | Simple algorithm do find a cycle in a graph, if any exists. This one can
-- still be optimised by a lot.
findCycle :: Ord a
          => DirectedGraph a
          -> Maybe [a]
findCycle dg = fmap reverse $ msum
    [ findCycle' [x] x n
    | x <- S.toList $ nodes dg
    , n <- neighbours x dg
    ]
  where
    findCycle' stack start x
        | x == start = Just (x : stack)
        | otherwise  = msum
            [ findCycle' (x : stack) start n
            | n <- neighbours x dg
            ]


--------------------------------------------------------------------------------
-- | Do not call this on graphs with cycles
findOrder :: Ord a
          => DirectedGraph a
          -> DirectedGraph a
          -> (a -> Bool)
          -> [a]
findOrder old new ood = ls
  where
    -- Make an extension of ood: an item is ood when it is actually ood OR if
    -- the list of its dependencies has changed. Based on that, create a set of
    -- dirty items.
    ood' x = ood x || neighbours x old /= neighbours x new
    dirty' = dirty ood' new

    -- Run all walks in our own little monad...
    (_, _, ls) = runRWS walks new dirty'


--------------------------------------------------------------------------------
type Analyzer i a = RWS (DirectedGraph i) [i] (Set i) a


--------------------------------------------------------------------------------
isDirty :: Ord a => a -> Analyzer a Bool
isDirty x = (x `S.member`) <$> get


--------------------------------------------------------------------------------
walks :: Ord a
      => Analyzer a ()
walks = do
    dirty' <- get
    if S.null dirty'
        then return ()
        else do
            walk $ S.findMin dirty'
            walks


--------------------------------------------------------------------------------
-- | Invariant: given node to walk /must/ be dirty
walk :: Ord a
     => a
     -> Analyzer a ()
walk x = do
    -- Determine dirty neighbours and walk them
    dg <- ask
    forM_ (neighbours x dg) $ \n -> do
        d <- isDirty n
        when d $ walk n

    -- Once all dirty neighbours are done, we're safe to go
    tell [x]
    modify $ S.delete x


--------------------------------------------------------------------------------
-- | This auxiliary function checks which nodes are dirty: a node is dirty if
-- it's out-of-date or if one of its dependencies is dirty.
dirty :: Ord a
      => (a -> Bool)      -- ^ Out of date?
      -> DirectedGraph a  -- ^ Graph
      -> Set a            -- ^ All dirty items
dirty ood dg = S.fromList $ flip evalState M.empty $
    filterM go $ S.toList $ nodes dg
  where
    go x = do
        m <- get
        case M.lookup x m of
            Just d  -> return d
            Nothing -> do
                nd <- mapM go $ neighbours x dg
                let d = ood x || or nd
                modify $ M.insert x d
                return d
