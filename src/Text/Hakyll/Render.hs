module Text.Hakyll.Render 
    ( depends,
      render,
      renderAndConcat,
      renderChain,
      static,
      css
    ) where

import Text.Template hiding (render)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Control.Monad

import System.Directory
import System.IO

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
render templatePath renderable = do
    handle <- openFile templatePath ReadMode
    templateString <- liftM B.pack $ hGetContents handle
    seq templateString $ hClose handle
    context <- toContext renderable
    let body = substitute templateString context
    return $ fromContext (M.insert (B.pack "body") body context)

-- | Render each renderable with the given template, then concatenate the
--   result.
renderAndConcat :: Renderable a => FilePath -> [a] -> IO B.ByteString
renderAndConcat templatePath renderables = foldM concatRender' B.empty renderables
    where concatRender' :: Renderable a => B.ByteString -> a -> IO B.ByteString
          concatRender' chunk renderable = do
              rendered <- render templatePath renderable
              let body = getBody rendered
              return $ B.append chunk $ body

-- | Chain a render action for a page with a number of templates. This will
--   also write the result to the site destination. This is the preferred way
--   to do general rendering.
renderChain :: Renderable a => [FilePath] -> a -> IO ()
renderChain templates renderable =
    depends (getURL renderable) (getDependencies renderable ++ templates) $
        do initialPage <- toContext renderable
           result <- foldM (flip render) (fromContext initialPage) templates
           writePage result

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
                    writeFile destination (compressCSS contents)
