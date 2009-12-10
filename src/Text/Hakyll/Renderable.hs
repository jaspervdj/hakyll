module Text.Hakyll.Renderable
    ( Renderable,
      toContext,
      getDependencies,
      getURL
    ) where

import System.FilePath
import Text.Template

class Renderable a where
    toContext :: a -> IO Context
    getDependencies :: a -> [FilePath]
    getURL :: a -> FilePath
