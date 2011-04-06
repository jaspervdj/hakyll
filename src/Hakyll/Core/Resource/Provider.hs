-- | This module provides an API for resource providers. Resource providers
-- allow Hakyll to get content from resources; the type of resource depends on
-- the concrete instance.
--
-- A resource is represented by the 'Resource' type. This is basically just a
-- newtype wrapper around 'Identifier' -- but it has an important effect: it
-- guarantees that a resource with this identifier can be provided by one or
-- more resource providers.
--
-- Therefore, it is not recommended to read files directly -- you should use the
-- provided 'Resource' methods.
--
module Hakyll.Core.Resource.Provider
    ( ResourceProvider (..)
    , resourceExists
    , resourceDigest
    , resourceModified
    ) where

import Control.Concurrent (MVar, readMVar, modifyMVar_)
import Control.Monad ((<=<))
import Data.Word (Word8)
import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.ByteString.Lazy as LB
import OpenSSL.Digest.ByteString.Lazy (digest)
import OpenSSL.Digest (MessageDigest (MD5))

import Hakyll.Core.Identifier
import Hakyll.Core.Store
import Hakyll.Core.Resource

-- | A value responsible for retrieving and listing resources
--
data ResourceProvider = ResourceProvider
    { -- | A list of all resources this provider is able to provide
      resourceList           :: [Resource]
    , -- | Retrieve a certain resource as string
      resourceString         :: Resource -> IO String
    , -- | Retrieve a certain resource as lazy bytestring
      resourceLazyByteString :: Resource -> IO LB.ByteString
    , -- | Cache keeping track of modified items
      resourceModifiedCache  :: MVar (Map Resource Bool)
    }

-- | Check if a given identifier has a resource
--
resourceExists :: ResourceProvider -> Identifier -> Bool
resourceExists provider = flip elem $ map unResource $ resourceList provider

-- | Retrieve a digest for a given resource
--
resourceDigest :: ResourceProvider -> Resource -> IO [Word8]
resourceDigest provider = digest MD5 <=< resourceLazyByteString provider

-- | Check if a resource was modified
--
resourceModified :: ResourceProvider -> Store -> Resource -> IO Bool
resourceModified provider store resource = do
    cache <- readMVar mvar
    case M.lookup resource cache of
        -- Already in the cache
        Just m  -> return m
        -- Not yet in the cache, check digests (if it exists)
        Nothing -> do
            m <- if resourceExists provider (unResource resource)
                        then digestModified provider store resource
                        else return False
            modifyMVar_ mvar (return . M.insert resource m)
            return m
  where
    mvar = resourceModifiedCache provider

-- | Check if a resource digest was modified
--
digestModified :: ResourceProvider -> Store -> Resource -> IO Bool
digestModified provider store resource = do
    -- Get the latest seen digest from the store
    lastDigest <- storeGet store itemName $ unResource resource
    -- Calculate the digest for the resource
    newDigest <- resourceDigest provider resource
    -- Check digests
    if Just newDigest == lastDigest
        -- All is fine, not modified
        then return False
        -- Resource modified; store new digest
        else do storeSet store itemName (unResource resource) newDigest
                return True
  where
    itemName = "Hakyll.Core.ResourceProvider.digestModified"
