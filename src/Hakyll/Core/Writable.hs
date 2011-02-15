-- | Describes writable items; items that can be saved to the disk
--
{-# LANGUAGE FlexibleInstances #-}
module Hakyll.Core.Writable
    ( Writable (..)
    ) where

import Data.Word (Word8)

import qualified Data.ByteString as SB

-- | Describes an item that can be saved to the disk
--
class Writable a where
    -- | Save an item to the given filepath
    write :: FilePath -> a -> IO ()

instance Writable [Char] where
    write = writeFile

instance Writable [Word8] where
    write p = SB.writeFile p . SB.pack
