-- | This (quite small) module exports the datatype used for contexts. A
--   @Context@ is a simple key-value mapping. You can render these @Context@s
--   with templates, and manipulate them in various ways.
module Text.Hakyll.Context
    ( Context
    ) where

import Data.Map (Map)

-- | Datatype used for key-value mappings.
type Context = Map String String
