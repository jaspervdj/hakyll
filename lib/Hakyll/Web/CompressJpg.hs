--------------------------------------------------------------------------------
-- | Module used for JPG compression. 
module Hakyll.Web.CompressJpg 
    ( compressJpgCompiler
    , compressJpg
    ) where

--------------------------------------------------------------------------------
import Data.ByteString.Lazy             (ByteString, toStrict)
import Codec.Picture.Jpg                (decodeJpeg, encodeJpegAtQuality)
import Codec.Picture.Saving             (imageToJpg)

--------------------------------------------------------------------------------
import Hakyll.Core.Item                 (Item)
import Hakyll.Core.Compiler             (Compiler, getResourceLBS)

type JpgQuality = Int

--------------------------------------------------------------------------------
-- | Compress a JPG bytestring to a certain quality setting
-- The quality should be between 0 (lowest quality) and 100 (best quality)
-- An error is raised if the image cannot be decoded.
compressJpg :: JpgQuality -> ByteString -> ByteString
compressJpg quality src = case im of
        Right im -> imageToJpg quality im
        Left _ -> error $ "Loading the image failed."
    -- The function `decodeJpeg` requires strict ByteString
    -- However, `imageToJpg` requires Lazy Bytestrings
    where im = (decodeJpeg . toStrict) src

--------------------------------------------------------------------------------
-- | Compiler that compresses a JPG image to a certain quality setting
-- The quality should be between 0 (lowest quality) and 100 (best quality)
-- An error is raised if the image cannot be decoded.
compressJpgCompiler :: JpgQuality -> Compiler (Item ByteString)
compressJpgCompiler quality = fmap (compressJpg quality) <$> getResourceLBS