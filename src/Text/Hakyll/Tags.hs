-- | Module containing some specialized functions to deal with tags.
module Text.Hakyll.Tags
    ( readTagMap
    ) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad

import Text.Hakyll.Util
import Text.Hakyll.Page

-- | Read a tag map. This creates a map from tags to page paths. This function
--   assumes the tags are located in the `tags` metadata field, separated by
--   commas.
readTagMap :: [FilePath] -> IO (M.Map String [FilePath])
readTagMap paths = foldM addPaths M.empty paths
    where addPaths current path = do
            page <- readPage path
            let tags = map trim $ split "," $ B.unpack $ getValue ("tags") page
            return $ foldr (\t -> M.insertWith (++) t [path]) current tags
