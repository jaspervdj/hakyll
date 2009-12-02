module Text.Hakyll.Util 
    ( touchDirectories,
      getRecursiveContents
    ) where

import System.Directory
import System.FilePath
import Control.Monad

touchDirectories :: FilePath -> IO ()
touchDirectories path = createDirectoryIfMissing True dir
    where dir = takeDirectory path

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)
