module Text.Hakyll.Render 
    ( depends,
      render,
      writePage,
      renderAndConcat,
      renderChain,
      static,
      staticDirectory
    ) where

import Text.Template hiding (render)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Control.Monad

import System.Directory
import System.IO

import Text.Hakyll.Page
import Text.Hakyll.Renderable
import Text.Hakyll.Util

depends :: FilePath -> [FilePath] -> IO () -> IO ()
depends file dependencies action = do
    valid <- isCacheValid (toDestination file) dependencies
    unless valid action

render :: Renderable a => FilePath -> a -> IO Page
render templatePath renderable = do
    handle <- openFile templatePath ReadMode
    templateString <- liftM B.pack $ hGetContents handle
    seq templateString $ hClose handle
    context <- toContext renderable
    let body = substitute templateString context
    return $ fromContext (M.insert (B.pack "body") body context)

writePage :: Page -> IO ()
writePage page = do
    let destination = toDestination $ getURL page
    makeDirectories destination
    B.writeFile destination (getBody page)

renderAndConcat :: FilePath -> [FilePath] -> IO B.ByteString
renderAndConcat templatePath paths = foldM concatRender' B.empty paths
    where concatRender' :: B.ByteString -> FilePath -> IO B.ByteString
          concatRender' chunk path = do
              page <- readPage path
              rendered <- render templatePath page
              let body = getBody rendered
              return $ B.append chunk $ body

renderChain :: Renderable a => [FilePath] -> a -> IO ()
renderChain templates renderable =
    depends (getURL renderable) (getDependencies renderable ++ templates) $
        do initialPage <- toContext renderable
           result <- foldM (flip render) (fromContext initialPage) templates
           writePage result

static :: FilePath -> IO ()
static source = do
    makeDirectories destination
    copyFile source destination
    where destination = toDestination source

staticDirectory :: FilePath -> IO ()
staticDirectory dir = 
    getRecursiveContents dir >>= mapM_ static
