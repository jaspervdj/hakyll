-- | A module for dealing with @Page@s. This module is mostly internally used.
module Text.Hakyll.Internal.Page 
    ( readPage
    ) where

import qualified Data.Map as M
import Data.List (isPrefixOf)
import Data.Char (isSpace)
import Control.Monad.Reader (liftIO)
import System.FilePath
import Control.Monad.State (State, evalState, get, put)

import Text.Pandoc

import Text.Hakyll.Context (Context)
import Text.Hakyll.File
import Text.Hakyll.HakyllMonad
import Text.Hakyll.Regex (substituteRegex, matchesRegex)
import Text.Hakyll.Util (trim)
import Text.Hakyll.Internal.Cache
import Text.Hakyll.Internal.FileType

-- | The default reader options for pandoc parsing.
readerOptions :: ParserState
readerOptions = defaultParserState
    { -- The following option causes pandoc to read smart typography, a nice
      -- and free bonus.
      stateSmart = True
    }

-- | The default writer options for pandoc rendering.
writerOptions :: WriterOptions
writerOptions = defaultWriterOptions
    { -- This option causes literate haskell to be written using '>' marks in
      -- html, which I think is a good default.
      writerLiterateHaskell = True
    }

-- | Get a render function for a given extension.
getRenderFunction :: FileType -> (String -> String)
getRenderFunction Html     = id
getRenderFunction Text     = id
getRenderFunction fileType = writeHtmlString writerOptions
                           . readFunction fileType (readOptions fileType)
  where
    readFunction ReStructuredText        = readRST
    readFunction LaTeX                   = readLaTeX
    readFunction Markdown                = readMarkdown
    readFunction LiterateHaskellMarkdown = readMarkdown
    readFunction t                       = error $ "Cannot render " ++ show t

    readOptions LiterateHaskellMarkdown =
        readerOptions { stateLiterateHaskell = True }
    readOptions _                       = readerOptions

-- | Split a page into sections.
splitAtDelimiters :: [String] -> State (Maybe String) [[String]]
splitAtDelimiters [] = return []
splitAtDelimiters ls@(x:xs) = do
    delimiter <- get
    if not (isDelimiter delimiter x)
        then return [ls]
        else do let proper = takeWhile (== '-') x
                    (content, rest) = break (isDelimiter $ Just proper) xs
                put $ Just proper
                rest' <- splitAtDelimiters rest
                return $ (x : content) : rest'
  where
    isDelimiter old = case old of
        Nothing  -> isPossibleDelimiter
        (Just d) -> (== d) . takeWhile (== '-')

-- | Check if the given string is a metadata delimiter.
isPossibleDelimiter :: String -> Bool
isPossibleDelimiter = isPrefixOf "---"

-- | Read one section of a page.
readSection :: (String -> String) -- ^ Render function.
            -> Bool -- ^ If this section is the first section in the page.
            -> [String] -- ^ Lines in the section.
            -> [(String, String)] -- ^ Key-values extracted.
readSection _ _ [] = []
readSection renderFunction isFirst ls
    | not isDelimiter' = body ls
    | isNamedDelimiter = readSectionMetaData ls
    | isFirst = readSimpleMetaData (tail ls)
    | otherwise = body (tail ls)
  where
    isDelimiter' = isPossibleDelimiter (head ls)
    isNamedDelimiter = head ls `matchesRegex` "^----*  *[a-zA-Z0-9][a-zA-Z0-9]*"
    body ls' = [("body", renderFunction $ unlines ls')]

    readSimpleMetaData = map readPair . filter (not . all isSpace)
    readPair = trimPair . break (== ':')
    trimPair (key, value) = (trim key, trim $ tail value)

    readSectionMetaData [] = []
    readSectionMetaData (header:value) =
        let key = substituteRegex "[^a-zA-Z0-9]" "" header
        in [(key, renderFunction $ unlines value)]

-- | Read a page from a file. Metadata is supported, and if the filename
--   has a @.markdown@ extension, it will be rendered using pandoc.
readPageFromFile :: FilePath -> Hakyll Context
readPageFromFile path = do
    let renderFunction = getRenderFunction $ getFileType path
        sectionFunctions = map (readSection renderFunction)
                               (True : repeat False)

    -- Read file.
    contents <- liftIO $ readFile path
    url <- toUrl path
    let sections = evalState (splitAtDelimiters $ lines contents) Nothing
        sectionsData = concat $ zipWith ($) sectionFunctions sections
        context = M.fromList $
            ("url", url) : ("path", path) : category ++ sectionsData

    return context
  where
    category = let dirs = splitDirectories $ takeDirectory path
               in [("category", last dirs) | not (null dirs)]

-- | Read a page. Might fetch it from the cache if available. Otherwise, it will
--   read it from the file given and store it in the cache.
readPage :: FilePath -> Hakyll Context
readPage path = do
    isCacheMoreRecent' <- isCacheMoreRecent fileName [path]
    if isCacheMoreRecent' then getFromCache fileName
                          else do page <- readPageFromFile path
                                  storeInCache page fileName
                                  return page
  where
    fileName = "pages" </> path
