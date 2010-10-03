-- | A module for dealing with @Page@s. This module is mostly internally used.
module Text.Hakyll.Page
    ( PageSection (..)
    , readPage
    , readPageAction
    ) where

import Data.List (isPrefixOf)
import Data.Char (isSpace)
import Control.Monad.Reader (liftIO)
import System.FilePath
import Control.Monad.State (State, evalState, get, put)

import Text.Hakyll.File
import Text.Hakyll.HakyllMonad
import Text.Hakyll.HakyllAction
import Text.Hakyll.Regex (substituteRegex, matchesRegex)
import Text.Hakyll.Util (trim)

-- | Page info handle: (key, value, needs rendering)
--
data PageSection = PageSection {unPageSection :: [(String, String, Bool)]}
                 deriving (Show)

-- | Split a page into sections.
--
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
--
readSection :: Bool -- ^ If this section is the first section in the page.
            -> [String] -- ^ Lines in the section.
            -> PageSection -- ^ Key-values extracted.
readSection _ [] = PageSection []
readSection isFirst ls
    | not isDelimiter' = body ls
    | isNamedDelimiter = PageSection $ readSectionMetaData ls
    | isFirst = PageSection $ readSimpleMetaData (drop 1 ls)
    | otherwise = body (drop 1 ls)
  where
    isDelimiter' = isPossibleDelimiter (head ls)
    isNamedDelimiter = head ls `matchesRegex` "^----*  *[a-zA-Z0-9][a-zA-Z0-9]*"
    body ls' = PageSection [("body", unlines ls', True)]

    readSimpleMetaData = map readPair . filter (not . all isSpace)
    readPair = trimPair . break (== ':')
    trimPair (key, value) = (trim key, trim (drop 1 value), False)

    readSectionMetaData [] = []
    readSectionMetaData (header:value) =
        let key = substituteRegex "[^a-zA-Z0-9]" "" header
        in [(key, unlines value, True)]

-- | Read a page from a file. Metadata is supported.
--
readPage :: FilePath -> Hakyll [PageSection]
readPage path = do
    let sectionFunctions = map readSection $ True : repeat False

    -- Read file.
    contents <- liftIO $ readFile path
    url <- toUrl path
    let sections = evalState (splitAtDelimiters $ lines contents) Nothing
        sectionsData = zipWith ($) sectionFunctions sections

    return $ PageSection [ ("url", url, False)
                         , ("path", path, False)
                         ] : category : sectionsData
  where
    category = let dirs = splitDirectories $ takeDirectory path
               in PageSection [("category", last dirs, False) | not (null dirs)]

-- | Read a page from a file. Metadata is supported.
--
readPageAction :: FilePath -> HakyllAction () [PageSection]
readPageAction path = HakyllAction
    { actionDependencies = [path]
    , actionUrl          = Left $ toUrl path
    , actionFunction     = const $ readPage path
    }
