-- | Module containing the template data structure
--
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Hakyll.Web.Template.Internal
    ( Template (..)
    , TemplateElement (..)
    ) where

import Control.Applicative ((<$>))

import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.Typeable (Typeable)

import Hakyll.Core.Writable

-- | Datatype used for template substitutions.
--
newtype Template = Template
    { unTemplate :: [TemplateElement]
    }
    deriving (Show, Eq, Binary, Typeable)

instance Writable Template where
    -- Writing a template is impossible
    write _ _ = return ()

-- | Elements of a template.
--
data TemplateElement
    = Chunk String
    | Identifier String
    | Escaped String
    deriving (Show, Eq, Typeable)

instance Binary TemplateElement where
    put (Chunk string)    = putWord8 0 >> put string
    put (Identifier key)  = putWord8 1 >> put key
    put (Escaped key) = putWord8 2 >> put key

    get = getWord8 >>= \tag -> case tag of
            0 -> Chunk      <$> get
            1 -> Identifier <$> get
            2 -> Escaped    <$> get
            _ -> error "Error reading cached template"
