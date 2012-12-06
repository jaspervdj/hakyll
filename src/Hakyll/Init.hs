--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Monad         (forM_)
import           System.Directory      (copyFile)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure)
import           System.FilePath       ((</>))


--------------------------------------------------------------------------------
import           Hakyll.Core.Util.File
import           Paths_hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs
    srcDir   <- getDataFileName "example"
    files    <- getRecursiveContents srcDir

    case args of
        [dstDir] -> forM_ files $ \file -> do
            let dst = dstDir </> file
                src = srcDir </> file
            putStrLn $ "Creating " ++ dst
            makeDirectories dst
            copyFile src dst
        _ -> do
            putStrLn $ "Usage: " ++ progName ++ " <directory>"
            exitFailure
