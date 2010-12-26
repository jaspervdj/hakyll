-- | This module provides an API for resource providers. Resource providers
-- allow Hakyll to get content from resources; the type of resource depends on
-- the concrete instance.
--
module Hakyll.Core.ResourceProvider
    ( ResourceProvider (..)
    , resourceDigest
    ) where

import Control.Monad ((<=<))
import Data.Word (Word8)

import qualified Data.ByteString.Lazy as LB
import OpenSSL.Digest.ByteString.Lazy (digest)
import OpenSSL.Digest (MessageDigest (MD5))

import Hakyll.Core.Identifier

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

-- | Retrieve a digest for a given resource
--
resourceDigest :: ResourceProvider -> Identifier -> IO [Word8]
resourceDigest provider = digest MD5 <=< resourceLazyByteString provider
