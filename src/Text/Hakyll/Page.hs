module Text.Hakyll.Page 
    ( Page,
      addContext,
      getURL,
      getBody,
      readPage,
      pageFromList,
      concatPages
    ) where

import qualified Data.Map as M
import qualified Data.List as L
import System.FilePath
import Data.Maybe
import Text.Pandoc

type Page = M.Map String String

addContext :: String -> String -> Page -> Page
addContext key value = M.insert key value

getURL :: Page -> String
getURL context = fromMaybe "404.html" $ M.lookup "url" context

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

readPage :: FilePath -> IO Page
readPage path = do
    content <- readFile path
    let context = extractContext content
        body = (if takeExtension path == ".markdown" then markdownToHTML else id)
               (getBody context)
        url = addExtension (dropExtension path) ".html"
    return $ addContext "url" url $ addContext "body" body $ context

pageFromList :: [(String, String)] -> Page
pageFromList = M.fromList

concatPages :: [Page] -> String
concatPages = concat . map getBody
