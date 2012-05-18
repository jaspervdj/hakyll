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
    , makeResourceProvider
    , resourceExists
    , resourceDigest
    , resourceModified
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (MVar, readMVar, modifyMVar_, newMVar)
import Data.Map (Map)
import qualified Data.Map as M

import Data.Time (UTCTime)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Hakyll.Core.Store
import Hakyll.Core.Resource

-- | A value responsible for retrieving and listing resources
--
data ResourceProvider = ResourceProvider
    { -- | A list of all resources this provider is able to provide
      resourceList             :: [Resource]
    , -- | Retrieve a certain resource as string
      resourceString           :: Resource -> IO String
    , -- | Retrieve a certain resource as lazy bytestring
      resourceLBS              :: Resource -> IO LB.ByteString
    , -- | Check when a resource was last modified
      resourceModificationTime :: Resource -> IO UTCTime
    , -- | Cache keeping track of modified items
      resourceModifiedCache    :: MVar (Map Resource Bool)
    }

-- | Create a resource provider
--
makeResourceProvider :: [Resource]                      -- ^ Resource list
                     -> (Resource -> IO String)         -- ^ String reader
                     -> (Resource -> IO LB.ByteString)  -- ^ ByteString reader
                     -> (Resource -> IO UTCTime)        -- ^ Time checker
                     -> IO ResourceProvider             -- ^ Resulting provider
makeResourceProvider l s b t = ResourceProvider l s b t <$> newMVar M.empty

-- | Check if a given identifier has a resource
--
resourceExists :: ResourceProvider -> Resource -> Bool
resourceExists provider = flip elem $ resourceList provider

-- | Retrieve a digest for a given resource
--
resourceDigest :: ResourceProvider -> Resource -> IO B.ByteString
resourceDigest provider = fmap MD5.hashlazy . resourceLBS provider

-- | Check if a resource was modified
--
resourceModified :: ResourceProvider -> Store -> Resource -> IO Bool
resourceModified provider store r = do
    cache <- readMVar mvar
    case M.lookup r cache of
        -- Already in the cache
        Just m  -> return m
        -- Not yet in the cache, check digests (if it exists)
        Nothing -> do
            m <- if resourceExists provider r
                        then digestModified provider store r
                        else return False
            modifyMVar_ mvar (return . M.insert r m)
            return m
  where
    mvar = resourceModifiedCache provider

-- | Check if a resource digest was modified
--
digestModified :: ResourceProvider -> Store -> Resource -> IO Bool
digestModified provider store r = do
    -- Get the latest seen digest from the store
    lastDigest <- storeGet store itemName identifier
    -- Calculate the digest for the resource
    newDigest <- resourceDigest provider r
    -- Check digests
    if Found newDigest == lastDigest
        -- All is fine, not modified
        then return False
        -- Resource modified; store new digest
        else do storeSet store itemName identifier newDigest
                return True
  where
    identifier = toIdentifier r
    itemName = "Hakyll.Core.ResourceProvider.digestModified"
