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

import System.FilePath (FilePath, takeExtension)
import System.IO

import Text.Hakyll.File
import Text.Hakyll.Util (trim)
import Text.Hakyll.Context (Context)
import Text.Hakyll.Renderable
import Text.Pandoc


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
    readFunction ".tex" = readLaTeX
    readFunction _      = readMarkdown

-- | Read metadata header from a file handle.
readMetaData :: Handle -> IO [(String, String)]
readMetaData handle = do
    line <- hGetLine handle
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
cachePage :: Page -> IO ()
cachePage page@(Page mapping) = do
    let destination = toCache $ getURL page
    makeDirectories destination
    handle <- openFile destination WriteMode
    hPutStrLn handle "---"
    mapM_ (writePair handle) $ M.toList $ M.delete "body" mapping
    hPutStrLn handle "---"
    hPutStr handle $ getBody page
    hClose handle
  where
    writePair h (k, v) = do hPutStr h $ k ++ ": " ++ v
                            hPutStrLn h ""

-- | Read a page from a file. Metadata is supported, and if the filename
--   has a .markdown extension, it will be rendered using pandoc. Note that
--   pages are not templates, so they should not contain $identifiers.
readPage :: FilePath -> IO Page
readPage pagePath = do
    -- Check cache.
    getFromCache <- isCacheValid cacheFile [pagePath]
    let path = if getFromCache then cacheFile else pagePath

    -- Read file.
    handle <- openFile path ReadMode
    line <- hGetLine handle
    (metaData, body) <-
        if isDelimiter line
            then do md <- readMetaData handle
                    b <- hGetContents handle
                    return (md, b)
            else do b <- hGetContents handle
                    return ([], line ++ "\n" ++ b)

    -- Render file
    let rendered = (renderFunction $ takeExtension path) body
        page = fromContext $ M.fromList $
            [ ("body", rendered)
            , ("url", url)
            , ("path", pagePath)
            ] ++ metaData

    seq (($|) id rdeepseq rendered) $ hClose handle

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
