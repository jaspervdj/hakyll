module Text.Hakyll.Render 
    ( depends,
      renderPage,
      writePage,
      renderAndConcat,
      renderChain,
      static,
      staticDirectory
    ) where

import Text.Template
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Control.Monad

import System.Directory
import System.IO

import Text.Hakyll.Page
import Text.Hakyll.Util

depends :: FilePath -> [FilePath] -> IO () -> IO ()
depends file dependencies action = do
    valid <- isCacheValid (toDestination file) dependencies
    unless valid action

createContext :: Page -> Context
createContext = M.fromList . map packPair . M.toList
    where packPair (a, b) = (B.pack a, b)

renderPage :: FilePath -> Page -> IO Page
renderPage templatePath page = do
    handle <- openFile templatePath ReadMode
    templateString <- liftM B.pack $ hGetContents handle
    seq templateString $ hClose handle
    let body = substitute templateString (createContext page)
    return $ M.insert "body" body page

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
              rendered <- renderPage templatePath page
              let body = getBody rendered
              return $ B.append chunk $ body

renderChain :: [FilePath] -> FilePath -> IO ()
renderChain templates pagePath = depends (toURL pagePath) (pagePath : templates) $
    do page <- readPage pagePath
       result <- foldM (flip renderPage) page templates
       writePage result

static :: FilePath -> IO ()
static source = do
    makeDirectories destination
    copyFile source destination
    where destination = toDestination source

staticDirectory :: FilePath -> IO ()
staticDirectory dir = 
    getRecursiveContents dir >>= mapM_ static
