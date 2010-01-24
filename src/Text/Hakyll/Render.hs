-- | Module containing rendering functions. All these functions are used to
--   render files to the @_site@ directory.
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

import Control.Monad (unless)
import Control.Monad.Reader (liftIO)
import System.Directory (copyFile)

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
    destination <- toDestination file
    valid <- isMoreRecent destination dependencies
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

-- | Render each renderable with the given templates, then concatenate the
--   result. So, basically this function:
--
--   * Takes every renderable.
--
--   * Renders every renderable with all given templates. This is comparable
--     with a renderChain action.
--
--   * Concatenates the result.
--
renderAndConcat :: Renderable a
                => [FilePath] -- ^ Templates to apply on every renderable.
                -> [a] -- ^ Renderables to render.
                -> Hakyll String
renderAndConcat = renderAndConcatWith id

-- | Render each renderable with the given templates, then concatenate the
--   result. This function allows you to specify a "ContextManipulation" to
--   apply on every "Renderable".
renderAndConcatWith :: Renderable a
                    => ContextManipulation
                    -> [FilePath]
                    -> [a]
                    -> Hakyll String
renderAndConcatWith manipulation templatePaths renderables = do
    templates <- liftIO $ mapM readFile templatePaths
    contexts <- mapM toContext renderables
    return $ pureRenderAndConcatWith manipulation templates contexts

-- | Chain a render action for a page with a number of templates. This will
--   also write the result to the site destination. This is the preferred way
--   to do general rendering.
--
--   > renderChain [ "templates/notice.html"
--   >             , "templates/default.html"
--   >             ] $ createPagePath "warning.html"
--
--   This code will first render @warning.html@ using @templates/notice.html@,
--   and will then render the result with @templates/default.html@.
renderChain :: Renderable a => [FilePath] -> a -> Hakyll ()
renderChain = renderChainWith id

-- | A more custom render chain that allows you to specify a
--   "ContextManipulation" which to apply on the context when it is read first.
renderChainWith :: Renderable a
                => ContextManipulation -> [FilePath] -> a -> Hakyll ()
renderChainWith manipulation templatePaths renderable =
    depends (getURL renderable) dependencies render'
  where
    dependencies = getDependencies renderable ++ templatePaths
    render' = do templates <- liftIO $ mapM readFile templatePaths
                 context <- toContext renderable
                 let result = pureRenderChainWith manipulation templates context
                 writePage $ fromContext result

-- | Mark a certain file as static, so it will just be copied when the site is
--   generated.
static :: FilePath -> Hakyll ()
static source = do destination <- toDestination source
                   depends destination [source] (action destination)
  where
    action destination = do makeDirectories destination
                            liftIO $ copyFile source destination

-- | Render a css file, compressing it.
css :: FilePath -> Hakyll ()
css source = do destination <- toDestination source
                depends destination [source] (css' destination)
  where
    css' destination = do contents <- liftIO $ readFile source
                          makeDirectories destination
                          liftIO $ writeFile destination (compressCSS contents)
