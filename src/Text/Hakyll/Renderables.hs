module Text.Hakyll.Renderables
    ( CustomPage
    , createCustomPage
    , PagePath
    , createPagePath
    ) where

import Text.Hakyll.Hakyll (Hakyll)
import System.FilePath (FilePath)
import qualified Data.Map as M
import Text.Hakyll.Page
import Text.Hakyll.Renderable
import Text.Hakyll.File

-- | A custom page.
data CustomPage = CustomPage 
    { url :: String,
      dependencies :: [FilePath],
      mapping :: [(String, Either String (Hakyll String))]
    }

-- | Create a custom page.
createCustomPage :: String -- ^ Destination of the page, relative to _site.
                 -> [FilePath] -- ^ Dependencies of the page.
                 -> [(String, Either String (Hakyll String))] -- ^ Key - value 
                                                              --   mapping.
                 -> CustomPage
createCustomPage = CustomPage

instance Renderable CustomPage where
    getDependencies = dependencies
    getURL = url
    toContext page = do
        values <- mapM (either (return) (>>= return) . snd) (mapping page)
        return $ M.fromList $ [ ("url", url page)
                              ] ++ zip (map fst $ mapping page) values 

-- | PagePath is a class that wraps a FilePath. This is used to render Pages
--   without reading them first through use of caching.
data PagePath = PagePath FilePath

-- | Create a PagePath from a FilePath.
createPagePath :: FilePath -> PagePath
createPagePath = PagePath

-- We can render filepaths
instance Renderable PagePath where
    getDependencies (PagePath path) = return path
    getURL (PagePath path) = toURL path
    toContext (PagePath path) = readPage path >>= toContext
