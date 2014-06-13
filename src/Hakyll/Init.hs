--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow         (first)
import           Control.Monad         (forM_)
import           Data.Char             (isAlphaNum, isNumber)
import           Data.List             (intercalate)
import           Data.Version          (Version(..))
import           System.Directory      (copyFile, canonicalizePath)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure)
import           System.FilePath       ((</>), splitDirectories)


--------------------------------------------------------------------------------
import           Hakyll.Core.Util.File
import           Paths_hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs
    srcDir   <- getDataFileName "example"
    files    <- getRecursiveContents (const $ return False) srcDir

    case args of
        [dstDir] -> do
            forM_ files $ \file -> do
                let dst = dstDir </> file
                    src = srcDir </> file
                putStrLn $ "Creating " ++ dst
                makeDirectories dst
                copyFile src dst
            -- canonicalizePath is safe because the destination
            -- directory should exist at this point
            canonicalizePath dstDir >>= createCabal
        _ -> do
            putStrLn $ "Usage: " ++ progName ++ " <directory>"
            exitFailure


createCabal :: FilePath -> IO ()
createCabal dstDir = do
        putStrLn $ "Creating " ++ name ++ ".cabal"
        writeFile (dstDir </> name ++ ".cabal") $ unlines [
            "name:               " ++ name
          , "version:            0.1.0.0"
          , "build-type:         Simple"
          , "cabal-version:      >= 1.10"
          , ""
          , "executable site"
          , "  main-is:          site.hs"
          , "  build-depends:    base == 4.*"
          , "                  , hakyll == " ++ version' ++ ".*"
          , "  ghc-options:      -threaded"
          , "  default-language: Haskell2010"
          ]
  where
    -- Major hakyll version
    version' = intercalate "." . take 2 . map show $ versionBranch version
    -- last is safe here as the path is canonicalised and "/" is just
    -- a very rare but possible corner case
    name = case last (splitDirectories  dstDir) of
        "/" -> fallbackName
        x   -> repair (fallbackName ++) id x
    -- Package name repair code comes from
    -- cabal-install.Distribution.Client.Init.Heuristics
    repair invalid valid x = case dropWhile (not . isAlphaNum) x of
        "" -> repairComponent ""
        x' -> let (c, r) = first repairComponent $ break (not . isAlphaNum) x'
              in c ++ repairRest r
      where repairComponent c | all isNumber c = invalid c
                              | otherwise = valid c
    repairRest = repair id ('-' :)
    fallbackName = "site"
