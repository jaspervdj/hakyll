-- | Module containing the template data structure.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Hakyll.Internal.Template.Template
    ( Template (..)
    , TemplateElement (..)
    ) where

import Control.Applicative ((<$>))

import Data.Binary (Binary, get, getWord8, put, putWord8)

-- | Datatype used for template substitutions.
--
newtype Template = Template { unTemplate :: [TemplateElement] }
                 deriving (Show, Eq, Binary)

-- | Elements of a template.
--
data TemplateElement = Chunk String
                     | Identifier String
                     | EscapeCharacter
                     deriving (Show, Eq)

instance Binary TemplateElement where
    put (Chunk string)    = putWord8 0 >> put string
    put (Identifier key)  = putWord8 1 >> put key
    put (EscapeCharacter) = putWord8 2

    get = getWord8 >>= \tag ->
         case tag of 0 -> Chunk <$> get
                     1 -> Identifier <$> get
                     2 -> return EscapeCharacter
                     _ -> error "Error reading cached template"
