-- | Module exporting the simple 'Resource' type
--
module Hakyll.Core.Resource
    ( Resource (..)
    ) where

import Hakyll.Core.Identifier

-- | A resource
--
-- Invariant: the resource specified by the given identifier must exist
--
newtype Resource = Resource {unResource :: Identifier}
                 deriving (Eq, Show, Ord)
