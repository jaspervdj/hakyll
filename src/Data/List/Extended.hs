module Data.List.Extended
    ( module Data.List
    , breakWhen
    ) where

import           Data.List

-- | Like 'break', but can act on the entire tail of the list.
breakWhen :: ([a] -> Bool) -> [a] -> ([a], [a])
breakWhen predicate = go []
  where
    go buf []                = (reverse buf, [])
    go buf (x : xs)
        | predicate (x : xs) = (reverse buf, x : xs)
        | otherwise          = go (x : buf) xs
