-- | A module containing various function for manipulating and examinating
--   files and directories.
module Text.Hakyll.File
    ( toDestination
    , toCache
    , toUrl
    , toRoot
    , inHakyllDirectory
    , removeSpaces
    , makeDirectories
    , getRecursiveContents
    , sortByBaseName
    , havingExtension
    , directory
    , isMoreRecent
    , isFileMoreRecent
    ) where

import System.Directory
import Control.Applicative ((<$>))
import System.FilePath
import System.Time (ClockTime)
import Control.Monad
import Data.List (isPrefixOf, sortBy)
import Data.Ord (comparing)
import Control.Monad.Reader (liftIO)

import Text.Hakyll.HakyllMonad
import Text.Hakyll.Internal.FileType (isRenderableFile)

-- | Auxiliary function to remove pathSeparators form the start. We don't deal
--   with absolute paths here. We also remove $root from the start.
removeLeadingSeparator :: FilePath -> FilePath
removeLeadingSeparator [] = []
removeLeadingSeparator path
    | head path' `elem` pathSeparators = tail path'
    | otherwise                        = path'
  where
    path' = if "$root" `isPrefixOf` path then drop 5 path
                                         else path

-- | Convert a relative filepath to a filepath in the destination
--   (default: @_site@).
toDestination :: FilePath -> Hakyll FilePath
toDestination url = do dir <- askHakyll siteDirectory
                       enableIndexUrl' <- askHakyll enableIndexUrl
                       let destination = if enableIndexUrl' && separatorEnd
                               then dir </> noSeparator </> "index.html"
                               else dir </> noSeparator
                       return destination
  where
    noSeparator = removeLeadingSeparator url
    separatorEnd = not (null url) && last url == '/'

-- | Convert a relative filepath to a filepath in the cache
--   (default: @_cache@).
toCache :: FilePath -> Hakyll FilePath
toCache path = do dir <- askHakyll cacheDirectory
                  return $ dir </> removeLeadingSeparator path

-- | Get the url for a given page. For most extensions, this would be the path
--   itself. It's only for rendered extensions (@.markdown@, @.rst@, @.lhs@ this
--   function returns a path with a @.html@ extension instead.
toUrl :: FilePath -> Hakyll FilePath
toUrl path = do enableIndexUrl' <- askHakyll enableIndexUrl
                -- If the file does not have a renderable extension, like for
                -- example favicon.ico, we don't have to change it at all.
                return $ if not (isRenderableFile path)
                            then path
                            -- If index url's are enabled, we create pick it
                            -- unless the page is an index already.
                            else if enableIndexUrl' && not isIndex
                                then indexUrl
                                else withSimpleHtmlExtension
  where
    isIndex = dropExtension (takeFileName path) == "index"
    withSimpleHtmlExtension = flip addExtension ".html" $ dropExtension path
    indexUrl = dropExtension path ++ "/"
                            

-- | Get the relative url to the site root, for a given (absolute) url
toRoot :: FilePath -> FilePath
toRoot = emptyException . joinPath . map parent . splitPath
       . takeDirectory . removeLeadingSeparator
  where
    parent = const ".."
    emptyException [] = "."
    emptyException x  = x

-- | Check if a file is in a Hakyll directory. With a Hakyll directory, we mean
-- a directory that should be "ignored" such as the @_site@ or @_cache@
-- directory.
--
-- Example:
--
-- > inHakyllDirectory "_cache/pages/index.html"
--
-- Result:
--
-- > True
--
inHakyllDirectory :: FilePath -> Hakyll Bool
inHakyllDirectory path =
    or <$> mapM (liftM inDirectory . askHakyll) [siteDirectory, cacheDirectory]
  where
    inDirectory dir = case splitDirectories path of
        [] -> False
        (x : _) -> x == dir

-- | Swaps spaces for '-'.
removeSpaces :: FilePath -> FilePath
removeSpaces = map swap
  where
    swap ' ' = '-'
    swap x   = x

-- | Given a path to a file, try to make the path writable by making
--   all directories on the path.
makeDirectories :: FilePath -> Hakyll ()
makeDirectories path = liftIO $ createDirectoryIfMissing True dir
  where
    dir = takeDirectory path

-- | Get all contents of a directory. Note that files starting with a dot (.)
--   will be ignored.
getRecursiveContents :: FilePath -> Hakyll [FilePath]
getRecursiveContents topdir = do
    names <- liftIO $ getDirectoryContents topdir
    let properNames = filter isProper names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- liftIO $ doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)
  where
    isProper = not . (== '.') . head

-- | Sort a list of filenames on the basename.
sortByBaseName :: [FilePath] -> [FilePath]
sortByBaseName = sortBy compareBaseName
  where
    compareBaseName = comparing takeFileName

-- | A filter that takes all file names with a given extension. Prefix the
--   extension with a dot:
--
--   > havingExtension ".markdown" [ "index.markdown"
--   >                             , "style.css"
--   >                             ] == ["index.markdown"]
havingExtension :: String -> [FilePath] -> [FilePath]
havingExtension extension = filter ((==) extension . takeExtension)

-- | Perform a Hakyll action on every file in a given directory.
directory :: (FilePath -> Hakyll ()) -> FilePath -> Hakyll ()
directory action dir = getRecursiveContents dir >>= mapM_ action

-- | Check if a timestamp is newer then a number of given files.
isMoreRecent :: ClockTime  -- ^ The time to check.
             -> [FilePath] -- ^ Dependencies of the cached file.
             -> Hakyll Bool
isMoreRecent _ [] = return True
isMoreRecent timeStamp depends = do
    dependsModified <- liftIO $ mapM getModificationTime depends
    return (timeStamp >= maximum dependsModified)

-- | Check if a file is newer then a number of given files.
isFileMoreRecent :: FilePath   -- ^ The cached file.
                 -> [FilePath] -- ^ Dependencies of the cached file.
                 -> Hakyll Bool
isFileMoreRecent file depends = do
    exists <- liftIO $ doesFileExist file
    if not exists
        then return False
        else do timeStamp <- liftIO $ getModificationTime file
                isMoreRecent timeStamp depends
