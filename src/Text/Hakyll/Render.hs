module Text.Hakyll.Render 
    ( depends
    , render
    , renderWith
    , renderAndConcat
    , renderAndConcatWith
    , renderChain
    , renderChainWith
    , static
    , css
    ) where

import Control.Monad (unless, mapM, foldM)

import System.Directory (copyFile)
import System.IO

import Text.Hakyll.Context (ContextManipulation)
import Text.Hakyll.Page
import Text.Hakyll.Renderable
import Text.Hakyll.File
import Text.Hakyll.CompressCSS

import Text.Hakyll.Render.Internal

-- | Execute an IO action only when the cache is invalid.
depends :: FilePath -- ^ File to be rendered or created.
        -> [FilePath] -- ^ Files the render depends on.
        -> IO () -- ^ IO action to execute when the file is out of date.
        -> IO ()
depends file dependencies action = do
    valid <- isCacheValid (toDestination file) dependencies
    unless valid action

-- | Render to a Page.
render :: Renderable a
       => FilePath -- ^ Template to use for rendering.
       -> a -- ^ Renderable object to render with given template.
       -> IO Page -- ^ The body of the result will contain the render.
render = renderWith id

-- | Render to a Page. This function allows you to manipulate the context
--   first.
renderWith :: Renderable a
           => ContextManipulation -- ^ Manipulation to apply on the context.
           -> FilePath -- ^ Template to use for rendering.
           -> a -- ^ Renderable object to render with given template.
           -> IO Page -- ^ The body of the result will contain the render.
renderWith manipulation templatePath renderable = do
    template <- readFile templatePath
    context <- toContext renderable
    return $ fromContext $ pureRenderWith manipulation template context

-- | Render each renderable with the given template, then concatenate the
--   result.
renderAndConcat :: Renderable a => FilePath -> [a] -> IO String
renderAndConcat = renderAndConcatWith id

-- | Render each renderable with the given template, then concatenate the
--   result. This function allows you to specify a "ContextManipulation" to
--   apply on every "Renderable".
renderAndConcatWith :: Renderable a
                    => ContextManipulation
                    -> FilePath
                    -> [a]
                    -> IO String
renderAndConcatWith manipulation templatePath renderables =
    foldM concatRender' [] renderables
  where
    concatRender' :: Renderable a => String -> a -> IO String
    concatRender' chunk renderable = do
        rendered <- renderWith manipulation templatePath renderable
        let body = getBody rendered
        return $ chunk ++ body

-- | Chain a render action for a page with a number of templates. This will
--   also write the result to the site destination. This is the preferred way
--   to do general rendering.
renderChain :: Renderable a => [FilePath] -> a -> IO ()
renderChain = renderChainWith id

-- | A more custom render chain that allows you to specify a
--   "ContextManipulation" which to apply on the context when it is read first.
renderChainWith :: Renderable a
                => ContextManipulation -> [FilePath] -> a -> IO ()
renderChainWith manipulation templatePaths renderable =
    depends (getURL renderable) (getDependencies renderable ++ templatePaths) $
        do templates <- mapM readFile templatePaths
           context <- toContext renderable
           let result = pureRenderChainWith manipulation templates context
           writePage $ fromContext result

-- | Mark a certain file as static, so it will just be copied when the site is
--   generated.
static :: FilePath -> IO ()
static source = depends destination [source] action
  where
    destination = toDestination source
    action = do makeDirectories destination
                copyFile source destination

-- | Render a css file, compressing it.
css :: FilePath -> IO ()
css source = depends destination [source] css'
  where
    destination = toDestination source
    css' = do contents <- readFile source
              makeDirectories destination
              writeFile destination (compressCSS contents)
