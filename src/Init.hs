--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow         (first)
import           Control.Monad         (forM, forM_)
import           Data.Char             (isAlphaNum, isNumber)
import           Data.List             (foldl', intercalate, isPrefixOf)
import           Data.Version          (Version (..))
import           System.Directory      (canonicalizePath, copyFile,
                                        doesFileExist,
                                        setPermissions, getPermissions, writable)
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
    srcDir   <- getDataFileName "example"
    files    <- getRecursiveContents (const $ return False) srcDir

    case args of
        -- When the argument begins with hyphens, it's more likely that the user
        -- intends to attempt some arguments like ("--help", "-h", "--version", etc.)
        -- rather than create directory with that name.
        -- If dstDir begins with hyphens, the guard will prevent it from creating
        -- directory with that name so we can fall to the second alternative
        -- which prints a usage info for user.
        [dstDir] | not ("-" `isPrefixOf` dstDir) ->
            createFiles False srcDir files dstDir
        ["-f", dstDir] ->
            createFiles True srcDir files dstDir
        _ -> do
            putStrLn $ "Usage: " ++ progName ++ " [-f] <directory>"
            exitFailure

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
                        -- On some systems, the source folder may be readonly,
                        -- and copyFile will therefore create a readonly project...
                        p <- getPermissions dst
                        setPermissions dst (p {writable = True})

                    putStrLn $ "Creating " ++ cabalPath
                    createCabal cabalPath name
                fs -> do
                    putStrLn $ "The following files will be overwritten:"
                    mapM_ putStrLn fs
                    putStrLn $ "Use -f to overwrite them"
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
  where
    -- Major hakyll version
    version' = intercalate "." . take 2 . map show $ versionBranch version
