--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           System.Directory      (copyFile)


--------------------------------------------------------------------------------
import           Hakyll.Core.Util.File
import           Paths_hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = do
    let dstDir = "."
    srcDir <- getDataFileName "example"
    files  <- getRecursiveContents srcDir
    print files
    return ()
