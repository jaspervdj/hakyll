-- | Module containing the template data structure.
--
module Text.Hakyll.Internal.Template.Template
    ( Template (..)
    ) where

import Control.Monad (liftM, liftM2)
import Data.Word (Word8)

import Data.Binary (Binary, get, put, getWord8)

-- | Datatype used for template substitutions.
--
data Template = Chunk String Template
              | Identifier String Template
              | EscapeCharacter Template
              | End
              deriving (Show, Read, Eq)
    
instance Binary Template where
    put (Chunk string template) = put (0 :: Word8) >> put string >> put template
    put (Identifier key template) = put (1 :: Word8) >> put key >> put template
    put (EscapeCharacter template) = put (2 :: Word8) >> put template
    put (End) = put (3 :: Word8)

    get = do tag <- getWord8
             case tag of 0 -> liftM2 Chunk get get
                         1 -> liftM2 Identifier get get
                         2 -> liftM EscapeCharacter get
                         3 -> return End
                         _ -> error "Error reading template"
