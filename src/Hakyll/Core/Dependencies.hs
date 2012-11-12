--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
module Hakyll.Core.Dependencies
    (
    ) where


--------------------------------------------------------------------------------
import           Data.List                      (foldl')
import           Data.Map                       (Map)
import qualified Data.Map                       as M
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
    -> (Set (Identifier ()), DependencyFacts)
outOfDate universe ood oldFacts = (ood, oldFacts)


--------------------------------------------------------------------------------
-- | Determine patterns with changed results
changedPatterns
    :: [Identifier ()]
    -> DependencyFacts
    -> (Set (Identifier ()), DependencyFacts)
changedPatterns universe facts =
    M.foldlWithKey' changed (S.empty, facts) facts
  where
    changed (!o, !f) id' deps =
        let (o', deps') = foldr (changed' id') (o, []) deps
        in (o', M.insert id' deps' f)

    changed' _   (Identifier i) (o, d) = (o, Identifier i : d)
    changed' id' (Pattern p ls) (o, d)
        | ls == ls' = (o, Pattern p ls : d)
        | otherwise = (S.insert id' o, Pattern p ls' : d)
      where
        ls' = filterMatches p universe
