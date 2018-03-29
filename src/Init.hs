--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow         (first)
import           Control.Monad         (forM, forM_)
import           Data.Char             (isAlphaNum, isNumber)
import           Data.List             (foldl', intercalate, (\\))
import           Data.Version          (Version (..))
import           System.Directory      (canonicalizePath, copyFile,
                                        doesFileExist)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure)
import           System.FilePath       (splitDirectories, (</>))


--------------------------------------------------------------------------------
import           Hakyll.Core.Util.File
import           Paths_hakyll


--------------------------------------------------------------------------------
import           Prelude


--------------------------------------------------------------------------------
main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs

    -- Retrieve all user arguments and flags.
    -- Example: hakyll-init --force template mypage
    let flags = filter (\x -> (head x) == '-') args
        arguments = args \\ flags

        isForced = (elem "-f" flags) || (elem "--force" flags)

        dstDir = case (length arguments) of
            0 -> []
            1 -> head arguments
            _ -> arguments !! 1

        exmDir = if (length arguments) < 2
            then "default"
            else head arguments


    srcDir <- getDataFileName $ "examples" </> exmDir
    files  <- getRecursiveContents (const $ return False) srcDir

    -- Makes sure that the destination location exists.
    if (length dstDir) < 1
        then do
            putStr $ unlines
                [ "Hakyll-" ++ version'
                , "Usage: " ++ progName
                  ++ " [-f|--force] [template] <directory>"
                ]
            exitFailure
        -- Makes sure that the template files exists.
        else if (length files) < 1
            then do
                putStrLn $ "Error: " ++ exmDir ++ " doesn't exists."
                exitFailure
            -- Check whether it is forced or not.
            else if isForced
                then createFiles True srcDir files dstDir
                else createFiles False srcDir files dstDir

    where
        createFiles force srcDir files dstDir = do
            name <- makeName dstDir
            let cabalPath = dstDir </> name ++ ".cabal"

            diff <- if force then return []
                    else existingFiles dstDir (cabalPath : files)

            case diff of
                [] -> do
                    forM_ files $ \file -> do
                        let dst = dstDir </> file
                            src = srcDir </> file
                        putStrLn $ "Creating " ++ dst
                        makeDirectories dst
                        copyFile src dst

                    putStrLn $ "Creating " ++ cabalPath
                    createCabal cabalPath name
                fs -> do
                    putStrLn $ "The following files will be overwritten:"
                    mapM_ putStrLn fs
                    putStrLn $ "Use -f or --force to overwrite them"
                    exitFailure

existingFiles :: FilePath -> [FilePath] -> IO [FilePath]
existingFiles dstDir files = fmap concat $ forM files $ \file -> do
    let dst = dstDir </> file
    exists <- doesFileExist dst
    return $ if exists then [dst] else []

-- | Figure out a good cabal package name from the given (existing) directory
-- name
makeName :: FilePath -> IO String
makeName dstDir = do
    canonical <- canonicalizePath dstDir
    return $ case safeLast (splitDirectories canonical) of
        Nothing  -> fallbackName
        Just "/" -> fallbackName
        Just x   -> repair (fallbackName ++) id x
  where
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

    safeLast = foldl' (\_ x -> Just x) Nothing

createCabal :: FilePath -> String -> IO ()
createCabal path name =
    writeFile path $ unlines [
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

-- Major hakyll version
version' :: String
version' = intercalate "." . take 2 . map show $ versionBranch version
