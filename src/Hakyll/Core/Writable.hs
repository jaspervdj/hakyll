-- | Describes writable items; items that can be saved to the disk
--
{-# LANGUAGE FlexibleInstances #-}
module Hakyll.Core.Writable
    ( Writable (..)
    ) where

-- | Describes an item that can be saved to the disk
--
class Writable a where
    -- | Save an item to the given filepath
    write :: FilePath -> a -> IO ()

instance Writable [Char] where
    write = writeFile
