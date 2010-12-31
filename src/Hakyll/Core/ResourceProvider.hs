-- | This module provides an API for resource providers. Resource providers
-- allow Hakyll to get content from resources; the type of resource depends on
-- the concrete instance.
--
module Hakyll.Core.ResourceProvider
    ( ResourceProvider (..)
    , resourceDigest
    , resourceModified
    ) where

import Control.Monad ((<=<))
import Data.Word (Word8)

import qualified Data.ByteString.Lazy as LB
import OpenSSL.Digest.ByteString.Lazy (digest)
import OpenSSL.Digest (MessageDigest (MD5))

import Hakyll.Core.Identifier
import Hakyll.Core.Store

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

-- | Check if a resource was modified
--
resourceModified :: ResourceProvider -> Identifier -> Store -> IO Bool
resourceModified provider identifier store = do
    -- Get the latest seen digest from the store
    lastDigest <- storeGet store itemName identifier
    -- Calculate the digest for the resource
    newDigest <- resourceDigest provider identifier
    -- Check digests
    if Just newDigest == lastDigest
        -- All is fine, not modified
        then return False
        -- Resource modified; store new digest
        else do storeSet store itemName identifier newDigest
                return True
  where
    itemName = "Hakyll.Core.ResourceProvider.resourceModified"
