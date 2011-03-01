-- | Module providing a function to parse a page from a file
--
module Hakyll.Web.Page.Read
    ( readPage
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second, (***))
import Control.Monad.State (State, get, put, evalState)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M

import Hakyll.Web.Page.Internal
import Hakyll.Core.Util.String

-- | We're using a simple state monad as parser
--
type LineParser = State [String]

-- | Read the metadata section from a page
--
parseMetadata :: LineParser (Map String String)
parseMetadata = get >>= \content -> case content of
    -- No lines means no metadata
    [] -> return M.empty
    -- Check if the file begins with a delimiter
    (l : ls) -> if not (isPossibleDelimiter l)
        then -- No delimiter means no metadata
             return M.empty
        else do -- Break the metadata section
                let (metadata, rest) = second (drop 1) $ break (== l) ls
                -- Put the rest back
                put rest
                -- Parse the metadata
                return $ M.fromList $ map parseMetadata' metadata
  where
    -- Check if a line can be a delimiter
    isPossibleDelimiter = isPrefixOf "---"

    -- Parse a "key: value" string to a (key, value) tupple
    parseMetadata' = (trim *** trim . drop 1) . break (== ':')

-- | Read the body section of a page
--
parseBody :: LineParser String
parseBody = do
    body <- get
    put []
    return $ unlines body

-- | Read an entire page
--
parsePage :: LineParser (Page String)
parsePage = Page <$> parseMetadata <*> parseBody

-- | Read a page from a string
--
readPage :: String -> Page String
readPage = evalState parsePage . lines
