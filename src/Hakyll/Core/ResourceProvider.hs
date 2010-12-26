-- | This module provides an API for resource providers. Resource providers
-- allow Hakyll to get content from resources; the type of resource depends on
-- the concrete instance.
--
module Hakyll.Core.ResourceProvider
    ( ResourceProvider (..)
    ) where

import Hakyll.Core.Identifier

import qualified Data.ByteString.Lazy as LB

-- | A value responsible for retrieving and listing resources
--
data ResourceProvider = ResourceProvider
    { -- | A list of all resources this provider is able to provide
      resourceList           :: [Identifier]
    , -- | Retrieve a certain resource as string
      resourceString         :: Identifier -> IO String
    , -- | Retrieve a certain resource as lazy bytestring
      resourceLazyByteString :: Identifier -> IO LB.ByteString
    }
