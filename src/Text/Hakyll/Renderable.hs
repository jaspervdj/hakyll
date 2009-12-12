module Text.Hakyll.Renderable
    ( Renderable(toContext, getDependencies, getURL)
    ) where

import System.FilePath
import Text.Template

-- | A class for datatypes that can be rendered to pages.
class Renderable a where
    -- | Get a context to do substitutions with.
    toContext :: a -> IO Context

    -- | Get the dependencies for the renderable. This is used for cache
    --   invalidation.
    getDependencies :: a -> [FilePath]

    -- | Get the destination for the renderable.
    getURL :: a -> FilePath
