module Text.Hakyll
    ( hakyll
    ) where

import System.Environment (getArgs, getProgName)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)

-- | Main function to run hakyll.
hakyll :: IO () -> IO ()
hakyll action = do
    args <- getArgs
    case args of []          -> action
                 ["--clean"] -> clean
                 _           -> showHelp

clean :: IO ()
clean = do remove' "_cache"
           remove' "_site"
    where remove' dir = do exists <- doesDirectoryExist dir
                           if exists then removeDirectoryRecursive dir
                                     else return ()

-- | Show usage information.
showHelp :: IO ()
showHelp = do
    name <- getProgName
    putStrLn $  "This is a hakyll site generator program. You should always run\n"
             ++ "it from the project root directory.\n"
             ++ "\n"
             ++ "Usage:\n"
             ++ name ++ "           Generate the site.\n"
             ++ name ++ " --clean   Clean up and remove cache.\n"
             ++ name ++ " --help    Show this message.\n"
