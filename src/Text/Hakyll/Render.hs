module Text.Hakyll.Render 
    ( renderPage,
      renderAndWrite,
      static,
      staticDirectory
    ) where

import Text.Template
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Control.Monad

import System.FilePath
import System.Directory

import Text.Hakyll.Page
import Text.Hakyll.Util

toDestination :: FilePath -> FilePath
toDestination path = "_site" </> path

createContext :: Page -> Context
createContext = M.fromList . map packPair . M.toList
    where packPair (a, b) = (B.pack a, B.pack b)

renderPage :: FilePath -> Page -> IO Page
renderPage templatePath page = do
    templateString <- B.readFile templatePath
    let body = substitute templateString (createContext page)
    return $ addContext "body" (B.unpack body) page

renderAndWrite :: FilePath -> Page -> IO ()
renderAndWrite templatePath page = do
    rendered <- renderPage templatePath page
    let destination = toDestination $ getURL rendered
    makeDirectories destination
    writeFile destination (getBody rendered)

static :: FilePath -> IO ()
static source = do
    makeDirectories destination
    copyFile source destination
    where destination = toDestination source

staticDirectory :: FilePath -> IO ()
staticDirectory dir = 
    getRecursiveContents dir >>= mapM_ static
