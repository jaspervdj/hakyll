module Text.Hakyll.RenderableFilePath
    ( RenderableFilePath (..)
    ) where

import System.FilePath
import Text.Hakyll.Renderable
import Text.Hakyll.Util
import Text.Hakyll.Page

newtype RenderableFilePath = RenderableFilePath FilePath

-- We can render filepaths
instance Renderable RenderableFilePath where
    getDependencies (RenderableFilePath path) = return path
    getURL (RenderableFilePath path) = toURL path
    toContext (RenderableFilePath path) = readPage path >>= toContext
