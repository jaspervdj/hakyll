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

import Text.Template hiding (render)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Control.Monad (unless, liftM, foldM)

import System.Directory (copyFile)
import System.IO

import Text.Hakyll.Context (ContextManipulation)
import Text.Hakyll.Page
import Text.Hakyll.Renderable
import Text.Hakyll.File
import Text.Hakyll.CompressCSS

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
    handle <- openFile templatePath ReadMode
    templateString <- liftM B.pack $ hGetContents handle
    seq templateString $ hClose handle
    context <- liftM manipulation $ toContext renderable
    -- Ignore $root when substituting here. We will only replace that in the
    -- final render (just before writing).
    let contextIgnoringRoot = M.insert (B.pack "root") (B.pack "$root") context
        body = substitute templateString contextIgnoringRoot
    return $ fromContext (M.insert (B.pack "body") body context)

-- | Render each renderable with the given template, then concatenate the
--   result.
renderAndConcat :: Renderable a => FilePath -> [a] -> IO B.ByteString
renderAndConcat = renderAndConcatWith id

-- | Render each renderable with the given template, then concatenate the
--   result. This function allows you to specify a "ContextManipulation" to
--   apply on every "Renderable".
renderAndConcatWith :: Renderable a
                    => ContextManipulation
                    -> FilePath
                    -> [a]
                    -> IO B.ByteString
renderAndConcatWith manipulation templatePath renderables =
    foldM concatRender' B.empty renderables
    where concatRender' :: Renderable a => B.ByteString -> a -> IO B.ByteString
          concatRender' chunk renderable = do
              rendered <- renderWith manipulation templatePath renderable
              let body = getBody rendered
              return $ B.append chunk $ body

-- | Chain a render action for a page with a number of templates. This will
--   also write the result to the site destination. This is the preferred way
--   to do general rendering.
renderChain :: Renderable a => [FilePath] -> a -> IO ()
renderChain = renderChainWith id

-- | A more custom render chain that allows you to specify a
--   "ContextManipulation" which to apply on the context when it is read first.
renderChainWith :: Renderable a
                => ContextManipulation -> [FilePath] -> a -> IO ()
renderChainWith manipulation templates renderable =
    depends (getURL renderable) (getDependencies renderable ++ templates) $
        do initialPage <- liftM manipulation $ toContext renderable
           result <- foldM (flip render) (fromContext initialPage) templates
           writePage result

-- | Write a page to the site destination.
writePage :: Page -> IO ()
writePage page = do
    let destination = toDestination url
    makeDirectories destination
    B.writeFile destination body
    where url = getURL page
          -- Substitute $root here, just before writing.
          body = substitute (getBody page)
                            (M.singleton (B.pack "root") (B.pack $ toRoot url))

-- | Mark a certain file as static, so it will just be copied when the site is
--   generated.
static :: FilePath -> IO ()
static source = depends destination [source]
        (makeDirectories destination >> copyFile source destination)
    where destination = toDestination source

-- | Render a css file, compressing it.
css :: FilePath -> IO ()
css source = depends destination [source] css'
    where destination = toDestination source
          css' = do h <- openFile source ReadMode
                    contents <- hGetContents h
                    makeDirectories destination
                    writeFile destination (compressCSS contents)
