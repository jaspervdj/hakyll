module Text.Hakyll.Renderables
    ( CustomPage,
      createCustomPage,
      PagePath,
      createPagePath
    ) where

import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Control.Monad
import Text.Hakyll.Page
import Text.Hakyll.Renderable
import Text.Hakyll.File

-- | A custom page.
data CustomPage = CustomPage 
    { url :: String,
      dependencies :: [FilePath],
      mapping :: [(String, Either String (IO B.ByteString))]
    }

-- | Create a custom page.
createCustomPage :: String -- ^ Destination of the page, relative to _site.
                 -> [FilePath] -- ^ Dependencies of the page.
                 -> [(String, Either String (IO B.ByteString))] -- ^ Key - value mapping for rendering.
                 -> CustomPage
createCustomPage = CustomPage

instance Renderable CustomPage where
    getDependencies = dependencies
    getURL = url
    toContext page = do
        values <- mapM (either (return . B.pack) (>>= return) . snd) (mapping page)
        let keys = map (B.pack . fst) (mapping page)
        return $ M.fromList $ (B.pack "url", B.pack $ url page) : zip keys values 

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
