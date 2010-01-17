module Text.Hakyll.Page 
    ( Page
    , fromContext
    , getValue
    , getBody
    , readPage
    ) where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Control.Parallel.Strategies (rdeepseq, ($|))
import Control.Monad.Reader (liftIO)
import System.FilePath (takeExtension)
import System.IO

import Text.Pandoc

import Text.Hakyll.Hakyll (Hakyll)
import Text.Hakyll.File
import Text.Hakyll.Util (trim)
import Text.Hakyll.Context (Context)
import Text.Hakyll.Renderable

-- | A Page is basically key-value mapping. Certain keys have special
--   meanings, like for example url, body and title.
data Page = Page Context

-- | Create a Page from a key-value mapping.
fromContext :: Context -> Page
fromContext = Page

-- | Obtain a value from a page. Will resturn an empty string when nothing is
--   found.
getValue :: String -> Page -> String
getValue str (Page page) = fromMaybe [] $ M.lookup str page

-- | Get the URL for a certain page. This should always be defined. If
--   not, it will error.
getPageURL :: Page -> String
getPageURL (Page page) = fromMaybe (error "No page url") $ M.lookup "url" page

-- | Get the original page path.
getPagePath :: Page -> String
getPagePath (Page page) =
    fromMaybe (error "No page path") $ M.lookup "path" page

-- | Get the body for a certain page. When not defined, the body will be
--   empty.
getBody :: Page -> String
getBody (Page page) = fromMaybe [] $ M.lookup "body" page

-- | The default writer options for pandoc rendering.
writerOptions :: WriterOptions
writerOptions = defaultWriterOptions

-- | Get a render function for a given extension.
renderFunction :: String -> (String -> String)
renderFunction ".html" = id
renderFunction ext = writeHtmlString writerOptions
                   . readFunction ext defaultParserState
  where
    readFunction ".markdown" = readMarkdown
    readFunction ".md"       = readMarkdown
    readFunction ".tex"      = readLaTeX
    readFunction _           = readMarkdown

-- | Read metadata header from a file handle.
readMetaData :: Handle -> Hakyll [(String, String)]
readMetaData handle = do
    line <- liftIO $ hGetLine handle
    if isDelimiter line
        then return []
        else do others <- readMetaData handle
                return $ (trimPair . break (== ':')) line : others
  where
    trimPair (key, value) = (trim key, trim $ tail value)

-- | Check if the given string is a metadata delimiter.
isDelimiter :: String -> Bool
isDelimiter = L.isPrefixOf "---"

-- | Used for caching of files.
cachePage :: Page -> Hakyll ()
cachePage page@(Page mapping) = do
    makeDirectories destination
    liftIO writePageToCache
  where
    (sectionMetaData, simpleMetaData) = M.partition (not . elem '\n')
                                                    (M.delete "body" mapping)

    writePageToCache = do
        handle <- openFile destination WriteMode
        hPutStrLn handle "---"
        mapM_ (writePair handle) $ M.toList simpleMetaData
        mapM_ (writeSection handle) $ M.toList sectionMetaData
        hPutStrLn handle "---"
        hPutStr handle $ getBody page
        hClose handle

    writePair h (k, v) = do hPutStr h $ k ++ ": " ++ v
                            hPutStrLn h ""

    writeSection h (k, v) = do hPutStrLn h $ "--- " ++ k
                               hPutStrLn h v

    destination = toCache $ getURL page

-- | Read a page from a file. Metadata is supported, and if the filename
--   has a @.markdown@ extension, it will be rendered using pandoc.
readPage :: FilePath -> Hakyll Page
readPage pagePath = do
    -- Check cache.
    getFromCache <- isCacheValid cacheFile [pagePath]
    let path = if getFromCache then cacheFile else pagePath

    -- Read file.
    handle <- liftIO $ openFile path ReadMode
    line <- liftIO $ hGetLine handle
    (metaData, body) <-
        if isDelimiter line
            then do md <- readMetaData handle
                    b <- liftIO $ hGetContents handle
                    return (md, b)
            else do b <- liftIO $ hGetContents handle
                    return ([], line ++ "\n" ++ b)

    -- Render file
    let rendered = (renderFunction $ takeExtension path) body
        page = fromContext $ M.fromList $
            [ ("body", rendered)
            , ("url", url)
            , ("path", pagePath)
            ] ++ metaData

    seq (($|) id rdeepseq rendered) $ liftIO $ hClose handle

    -- Cache if needed
    if getFromCache then return () else cachePage page
    return page
  where
    url = toURL pagePath
    cacheFile = toCache url

-- Make pages renderable.
instance Renderable Page where
    getDependencies = (:[]) . getPagePath
    getURL = getPageURL
    toContext (Page page) = return page
