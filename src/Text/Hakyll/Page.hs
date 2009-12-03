module Text.Hakyll.Page 
    ( Page,
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
import Data.Maybe

import System.FilePath
import System.IO

import Text.Pandoc

-- | A Page is basically key-value mapping. Certain keys have special
--   meanings, like for example url, body and title.
type Page = M.Map String String

-- | Add a key-value mapping to the Page.
addContext :: String -> String -> Page -> Page
addContext = M.insert

-- | Get the URL for a certain page. This should always be defined. If
--   not, it will return trash.html.
getURL :: Page -> String
getURL context = fromMaybe "trash.html" $ M.lookup "url" context

-- | Get the body for a certain page. When not defined, the body will be
--   empty.
getBody :: Page -> String
getBody context = fromMaybe "" $ M.lookup "body" context

readConfig :: [String] -> Page
readConfig = M.fromList . map (trim . break (== ':'))
    where trim (key, value) = (key, dropWhile (`elem` ": ") value)

extractContext :: String -> Page
extractContext str = M.insert "body" (unlines body) (readConfig header)
    where allLines = lines str
          isDelimiter = L.isPrefixOf "---"
          (header, body) | isDelimiter (head allLines) = let (h, b) = L.break (isDelimiter) (tail allLines)
                                                         in (h, tail b)
                         | otherwise = ([], allLines)

writerOptions :: WriterOptions
writerOptions = defaultWriterOptions

markdownToHTML :: String -> String
markdownToHTML = writeHtmlString writerOptions .
                 readMarkdown defaultParserState

-- | Read a page from a file. Metadata is supported, and if the filename
--   has a .markdown extension, it will be rendered using pandoc. Note that
--   pages are not templates, so they should not contain $identifiers.
readPage :: FilePath -> IO Page
readPage path = do
    handle <- openFile path ReadMode
    content <- hGetContents handle
    seq content $ hClose handle
    let context = extractContext content
        body = (if takeExtension path == ".markdown" then markdownToHTML else id)
               (getBody context)
        url = addExtension (dropExtension path) ".html"
    return $ addContext "url" url $ addContext "body" body $ context

-- | Create a key-value mapping page from an association list.
pageFromList :: [(String, String)] -> Page
pageFromList = M.fromList

-- | Concat the bodies of pages, and return the result.
concatPages :: [Page] -> String
concatPages = concatPagesWith "body"

-- | Concat certain values of pages, and return the result.
concatPagesWith :: String -- ^ Key of which to concat the values.
                -> [Page] -- ^ Pages to get the values from.
                -> String -- ^ The concatenation.
concatPagesWith key = concat . map (fromMaybe "" . M.lookup key)
