-- | Module for a simple static configuration of a website.
--
-- The configuration works like this:
--
-- * The @templates/@ directory should contain one template.
--
-- * Renderable files in the directory tree are rendered using this template.
--
-- * The @static/@ directory is copied entirely (if it exists).
--
-- * All files in the @css/@ directory are compressed.
--
module Text.Hakyll.Configurations.Static
    ( staticConfiguration
    ) where

import Control.Applicative ((<$>))
import Control.Monad (filterM, forM_)

import Text.Hakyll.File ( getRecursiveContents, inDirectory, inHakyllDirectory
                        , directory )
import Text.Hakyll.Internal.FileType (isRenderableFile)
import Text.Hakyll.HakyllMonad (Hakyll, logHakyll)
import Text.Hakyll.Render (renderChain, css, static)
import Text.Hakyll.CreateContext (createPage)

-- | A simple configuration for an entirely static website.
--
staticConfiguration :: Hakyll ()
staticConfiguration = do
    -- Find all files not in _site or _cache.
    files <- filterM isRenderableFile' =<< getRecursiveContents "."

    -- Find a main template to use
    mainTemplate <- take 1 <$> getRecursiveContents templateDir
    logHakyll $ case mainTemplate of [] ->      "Using no template"
                                     (x : _) -> "Using template " ++ x

    -- Render all files using this template
    forM_ files $ renderChain mainTemplate . createPage

    -- Render a static directory
    directory static staticDir

    -- Render a css directory
    directory css cssDir
  where
    -- A file should have a renderable extension and not be in a hakyll
    -- directory, and not in a special directory.
    isRenderableFile' file = do
        inHakyllDirectory' <- inHakyllDirectory file
        return $  isRenderableFile file
               && not (any (inDirectory file) [templateDir, cssDir, staticDir])
               && not inHakyllDirectory'

    -- Directories
    templateDir = "templates"
    cssDir = "css"
    staticDir = "static"
