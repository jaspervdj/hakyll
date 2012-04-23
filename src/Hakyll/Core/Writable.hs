-- | Describes writable items; items that can be saved to the disk
--
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Hakyll.Core.Writable
    ( Writable (..)
    ) where

import Data.Word (Word8)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)

import Hakyll.Core.Identifier

-- | Describes an item that can be saved to the disk
--
class Writable a where
    -- | Save an item to the given filepath
    write :: FilePath -> a -> IO ()

instance Writable () where
    write _ _ = return ()

instance Writable [Char] where
    write = writeFile

instance Writable SB.ByteString where
    write p = SB.writeFile p

instance Writable LB.ByteString where
    write p = LB.writeFile p

instance Writable [Word8] where
    write p = write p . SB.pack

instance Writable Html where
    write p html = write p $ renderHtml html

instance Writable (Identifier a) where
    write p = write p . show
