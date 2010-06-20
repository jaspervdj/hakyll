-- | This (quite small) module exports the datatype used for contexts. A
--   @Context@ is a simple key-value mapping. You can render these @Context@s
--   with templates, and manipulate them in various ways.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Hakyll.Context
    ( Context (..)
    ) where

import Data.Monoid (Monoid)
import Data.Map (Map)
import Data.Binary (Binary)

-- | Datatype used for key-value mappings.
newtype Context = Context { -- | Extract the context.
                            unContext :: Map String String
                          } deriving (Show, Monoid, Binary)
