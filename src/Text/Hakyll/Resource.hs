-- | A resource represents data for a website
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Hakyll.Resource
    ( Metadata (..)
    , Resource (..)
    , getData
    , getMetadata
    , getMetadataList
    ) where

import Data.Monoid (Monoid, mempty, mappend)
import Control.Applicative (Applicative, (<*>), pure)

-- | Metadata for a resource
--
newtype Metadata = Metadata {unMetadata :: [(String, String)]}
                 deriving (Show, Eq, Ord, Monoid)

-- | A resource represents a data source for the website. It contains a value
-- and a number of metadata fields
--
data Resource a = Resource
    { resourceMetadata :: Metadata
    , resourceData     :: a
    } deriving (Show, Eq, Ord)

instance Functor Resource where
    fmap f (Resource m d) = Resource m $ f d

instance Applicative Resource where
    pure d = Resource mempty d
    (Resource m1 f) <*> (Resource m2 d) = Resource (mappend m2 m1) (f d)

instance Monad Resource where
    return d = Resource mempty d
    (Resource m1 d) >>= f = let Resource m2 d' = f d
                            in Resource (mappend m2 m1) d'

instance Monoid a => Monoid (Resource a) where
    mempty = Resource mempty mempty
    mappend (Resource m1 d1) (Resource m2 d2) =
        Resource (mappend m1 m2) (mappend d1 d2)

-- | Get the data from a resource
--
getData :: Resource a -> a
getData = resourceData

-- | Get a metadata field from a resource
--
getMetadata :: String -> Resource a -> Maybe String
getMetadata k (Resource m _) = lookup k $ unMetadata m

-- | Get a metadata field from a resource. If multiple fields with the same name
-- exist, they will all be returned
--
getMetadataList :: String -> Resource a -> [String]
getMetadataList k = map snd . filter ((== k) . fst)
                  . unMetadata . resourceMetadata
