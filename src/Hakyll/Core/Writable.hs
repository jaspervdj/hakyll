-- | Describes writable items; items that can be saved to the disk
--
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
        DeriveDataTypeable #-}
module Hakyll.Core.Writable
    ( Writable (..)
    , CopyFile (..)
    ) where

import System.Directory (copyFile)
import Data.Word (Word8)

import qualified Data.ByteString as SB
import Data.Binary (Binary)
import Data.Typeable (Typeable)

-- | Describes an item that can be saved to the disk
--
class Writable a where
    -- | Save an item to the given filepath
    write :: FilePath -> a -> IO ()

instance Writable [Char] where
    write = writeFile

instance Writable [Word8] where
    write p = SB.writeFile p . SB.pack

-- | Newtype construct around 'FilePath' which will copy the file directly
--
newtype CopyFile = CopyFile {unCopyFile :: FilePath}
                 deriving (Show, Eq, Ord, Binary, Typeable)

instance Writable CopyFile where
    write dst (CopyFile src) = copyFile src dst
