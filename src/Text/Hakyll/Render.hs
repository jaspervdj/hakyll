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

import Control.Monad (unless, mapM)
import Control.Monad.Reader (liftIO)

import System.Directory (copyFile)
import System.IO

import Text.Hakyll.Hakyll (Hakyll)
import Text.Hakyll.Context (ContextManipulation)
import Text.Hakyll.Page
import Text.Hakyll.Renderable
import Text.Hakyll.File
import Text.Hakyll.CompressCSS

import Text.Hakyll.Render.Internal

-- | Execute an IO action only when the cache is invalid.
depends :: FilePath -- ^ File to be rendered or created.
        -> [FilePath] -- ^ Files the render depends on.
        -> Hakyll () -- ^ IO action to execute when the file is out of date.
        -> Hakyll ()
depends file dependencies action = do
    valid <- liftIO $ isCacheValid (toDestination file) dependencies
    unless valid action

-- | Render to a Page.
render :: Renderable a
       => FilePath -- ^ Template to use for rendering.
       -> a -- ^ Renderable object to render with given template.
       -> Hakyll Page -- ^ The body of the result will contain the render.
render = renderWith id

-- | Render to a Page. This function allows you to manipulate the context
--   first.
renderWith :: Renderable a
           => ContextManipulation -- ^ Manipulation to apply on the context.
           -> FilePath -- ^ Template to use for rendering.
           -> a -- ^ Renderable object to render with given template.
           -> Hakyll Page -- ^ The body of the result will contain the render.
renderWith manipulation templatePath renderable = do
    template <- liftIO $ readFile templatePath
    context <- toContext renderable
    return $ fromContext $ pureRenderWith manipulation template context

-- | Render each renderable with the given template, then concatenate the
--   result.
renderAndConcat :: Renderable a => FilePath -> [a] -> Hakyll String
renderAndConcat = renderAndConcatWith id

-- | Render each renderable with the given template, then concatenate the
--   result. This function allows you to specify a "ContextManipulation" to
--   apply on every "Renderable".
renderAndConcatWith :: Renderable a
                    => ContextManipulation
                    -> FilePath
                    -> [a]
                    -> Hakyll String
renderAndConcatWith manipulation templatePath renderables = do
    template <- liftIO $ readFile templatePath
    contexts <- mapM toContext renderables
    return $ pureRenderAndConcatWith manipulation template contexts

-- | Chain a render action for a page with a number of templates. This will
--   also write the result to the site destination. This is the preferred way
--   to do general rendering.
renderChain :: Renderable a => [FilePath] -> a -> Hakyll ()
renderChain = renderChainWith id

-- | A more custom render chain that allows you to specify a
--   "ContextManipulation" which to apply on the context when it is read first.
renderChainWith :: Renderable a
                => ContextManipulation -> [FilePath] -> a -> Hakyll ()
renderChainWith manipulation templatePaths renderable =
    depends (getURL renderable) dependencies render'
  where
    dependencies = (getDependencies renderable) ++ templatePaths
    render' = do templates <- liftIO $ mapM readFile templatePaths
                 context <- toContext renderable
                 let result = pureRenderChainWith manipulation templates context
                 writePage $ fromContext result

-- | Mark a certain file as static, so it will just be copied when the site is
--   generated.
static :: FilePath -> Hakyll ()
static source = depends destination [source] (liftIO action)
  where
    destination = toDestination source
    action = do makeDirectories destination
                copyFile source destination

-- | Render a css file, compressing it.
css :: FilePath -> Hakyll ()
css source = depends destination [source] (liftIO css')
  where
    destination = toDestination source
    css' = do contents <- readFile source
              makeDirectories destination
              writeFile destination (compressCSS contents)
