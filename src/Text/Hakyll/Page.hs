module Text.Hakyll.Page 
    ( Page,
      PageValue,
      addContext,
      getURL,
      getBody,
      readPage,
      pageFromList,
      concatPages,
      concatPagesWith
    ) where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe

import System.FilePath
import System.IO

import Text.Pandoc

-- | A Page is basically key-value mapping. Certain keys have special
--   meanings, like for example url, body and title.
type Page = M.Map String PageValue

-- | We use a ByteString for obvious reasons.
type PageValue = B.ByteString


-- | Add a key-value mapping to the Page.
addContext :: String -> String -> Page -> Page
addContext key value = M.insert key (B.pack value)

-- | Get the URL for a certain page. This should always be defined. If
--   not, it will return trash.html.
getURL :: Page -> String
getURL context = let result = M.lookup "url" context
                 in case result of (Just url) -> B.unpack url
                                   Nothing    -> error "URL is not defined."

-- | Get the body for a certain page. When not defined, the body will be
--   empty.
getBody :: Page -> PageValue
getBody context = fromMaybe B.empty $ M.lookup "body" context

writerOptions :: WriterOptions
writerOptions = defaultWriterOptions

renderFunction :: String -> (String -> String)
renderFunction ".html" = id
renderFunction ext = writeHtmlString writerOptions .
                     renderFunction' ext defaultParserState
    where renderFunction' ".markdown" = readMarkdown
          renderFunction' ".md"       = readMarkdown
          renderFunction' ".tex"      = readLaTeX
          renderFunction' _           = readMarkdown

readMetaData :: Handle -> IO [(String, String)]
readMetaData handle = do
    line <- hGetLine handle
    if isDelimiter line then return []
                        else do others <- readMetaData handle
                                return $ (trim . break (== ':')) line : others
        where trim (key, value) = (key, dropWhile (`elem` ": ") value)

isDelimiter :: String -> Bool
isDelimiter = L.isPrefixOf "---"

-- | Read a page from a file. Metadata is supported, and if the filename
--   has a .markdown extension, it will be rendered using pandoc. Note that
--   pages are not templates, so they should not contain $identifiers.
readPage :: FilePath -> IO Page
readPage path = do
    handle <- openFile path ReadMode
    line <- hGetLine handle
    (context, body) <- if isDelimiter line
                            then do md <- readMetaData handle
                                    c <- hGetContents handle
                                    return (md, c)
                            else hGetContents handle >>= \b -> return ([], line ++ b)

    let rendered = B.pack $ (renderFunction $ takeExtension path) body
        url = addExtension (dropExtension path) ".html"
    seq rendered $ hClose handle
    return $ M.insert "body" rendered $ addContext "url" url $ pageFromList context

-- | Create a key-value mapping page from an association list.
pageFromList :: [(String, String)] -> Page
pageFromList = M.fromList . map packPair
    where packPair (k, v) = let pv = B.pack v
                            in seq pv (k, pv)

-- | Concat the bodies of pages, and return the result.
concatPages :: [Page] -> PageValue
concatPages = concatPagesWith "body"

-- | Concat certain values of pages, and return the result.
concatPagesWith :: String -- ^ Key of which to concat the values.
                -> [Page] -- ^ Pages to get the values from.
                -> PageValue -- ^ The concatenation.
concatPagesWith key = B.concat . map (fromMaybe B.empty . M.lookup key)
