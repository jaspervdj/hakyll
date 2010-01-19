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
import Text.Hakyll.Regex (substituteRegex, matchesRegex)

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

-- | The default reader options for pandoc parsing.
readerOptions :: ParserState
readerOptions = defaultParserState { stateSmart = True }

-- | The default writer options for pandoc rendering.
writerOptions :: WriterOptions
writerOptions = defaultWriterOptions

-- | Get a render function for a given extension.
getRenderFunction :: String -> (String -> String)
getRenderFunction ".html" = id
getRenderFunction ext = writeHtmlString writerOptions
                      . readFunction ext readerOptions
  where
    readFunction ".rst" = readRST
    readFunction ".tex" = readLaTeX
    readFunction _      = readMarkdown

-- | Split a page into sections.
splitAtDelimiters :: [String] -> [[String]]
splitAtDelimiters [] = []
splitAtDelimiters ls@(x:xs)
    | isDelimiter x = let (content, rest) = break isDelimiter xs
                      in (x : content) : splitAtDelimiters rest
    | otherwise = [ls]

-- | Check if the given string is a metadata delimiter.
isDelimiter :: String -> Bool
isDelimiter = L.isPrefixOf "---"

-- | Used for caching of files.
cachePage :: Page -> Hakyll ()
cachePage page@(Page mapping) = do
    makeDirectories destination
    liftIO writePageToCache
  where
    (sectionMetaData, simpleMetaData) = M.partition (elem '\n')
                                                    (M.delete "body" mapping)

    writePageToCache = do
        handle <- openFile destination WriteMode
        hPutStrLn handle "---"
        mapM_ (writePair handle) $ M.toList simpleMetaData
        mapM_ (writeSection handle) $ M.toList sectionMetaData
        hPutStrLn handle "---"
        hPutStrLn handle $ getBody page
        hClose handle

    writePair h (k, v) = do hPutStr h $ k ++ ": " ++ v
                            hPutStrLn h ""

    writeSection h (k, v) = do hPutStrLn h $ "--- " ++ k
                               hPutStrLn h v

    destination = toCache $ getURL page

-- | Read one section of a page.
readSection :: (String -> String) -- ^ Render function.
            -> Bool -- ^ If this section is the first section in the page.
            -> [String] -- ^ Lines in the section.
            -> [(String, String)] -- ^ Key-values extracted.
readSection _ _ [] = []
readSection renderFunction True ls
    | isDelimiter (head ls) = readSimpleMetaData (tail ls)
    | otherwise = [("body", renderFunction $ unlines ls)]
  where
    readSimpleMetaData = map readPair
    readPair = (trimPair . break (== ':'))
    trimPair (key, value) = (trim key, trim $ tail value)

readSection renderFunction False ls
    | isDelimiter (head ls) = readSectionMetaData ls
    | otherwise = error $ "Page parsing error at: " ++ head ls
  where
    readSectionMetaData [] = []
    readSectionMetaData (header:value) =
        let key = if header `matchesRegex` "----*  *[a-zA-Z][a-zA-Z]*"
                      then substituteRegex "[^a-zA-Z]" "" header
                      else "body"
        in [(key, renderFunction $ unlines value)]

-- | Read a page from a file. Metadata is supported, and if the filename
--   has a @.markdown@ extension, it will be rendered using pandoc.
readPage :: FilePath -> Hakyll Page
readPage pagePath = do
    -- Check cache.
    getFromCache <- isCacheValid cacheFile [pagePath]
    let path = if getFromCache then cacheFile else pagePath
        renderFunction = getRenderFunction $ takeExtension path
        sectionFunctions = map (readSection renderFunction)
                               (True : repeat False)

    -- Read file.
    handle <- liftIO $ openFile path ReadMode
    sections <- fmap (splitAtDelimiters . lines )
                     (liftIO $ hGetContents handle)

    let context = concat $ zipWith ($) sectionFunctions sections
        page = fromContext $ M.fromList $
            [ ("url", url)
            , ("path", pagePath)
            ] ++ context

    seq (($|) id rdeepseq context) $ liftIO $ hClose handle

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
