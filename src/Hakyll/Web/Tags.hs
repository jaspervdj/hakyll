{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Web.Tags
    ( Tags (..)
    , readTagsWith
    , readTags
    , readCategories
    ) where

import Control.Applicative ((<$>))
import Data.Map (Map)
import qualified Data.Map as M

import Data.Typeable (Typeable)
import Data.Binary (Binary, get, put)

import Hakyll.Web.Page
import Hakyll.Web.Util.String
import Hakyll.Core.Writable

-- | Data about tags
--
data Tags a = Tags
    { tagsMap :: Map String [Page a]
    } deriving (Show, Typeable)

instance Binary a => Binary (Tags a) where
    get = Tags <$> get
    put (Tags m) = put m

instance Writable (Tags a) where
    write _ _ = return ()

-- | Higher-level function to read tags
--
readTagsWith :: (Page a -> [String])  -- ^ Function extracting tags from a page
             -> [Page a]              -- ^ Pages
             -> Tags a                -- ^ Resulting tags
readTagsWith f pages = Tags
    { tagsMap = foldl (M.unionWith (++)) M.empty (map readTagsWith' pages)
    }
  where
    -- Create a tag map for one page
    readTagsWith' page =
        let tags = f page
        in M.fromList $ zip tags $ repeat [page]

-- | Read a tagmap using the @tags@ metadata field
--
readTags :: [Page a] -> Tags a
readTags = readTagsWith $ map trim . splitAll "," . getField "tags"

-- | Read a tagmap using the @category@ metadata field
--
readCategories :: [Page a] -> Tags a
readCategories = readTagsWith $ return . getField "category"
