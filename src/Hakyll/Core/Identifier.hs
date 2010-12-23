-- | An identifier is a type used to uniquely identify a resource, target...
--
-- One can think of an identifier as something similar to a file path. An
-- identifier is a path as well, with the different elements in the path
-- separated by @/@ characters. Examples of identifiers are:
--
-- * @posts/foo.markdown@
--
-- * @index@
--
-- * @error/404@
--
module Hakyll.Core.Identifier
    ( Identifier (..)
    , parseIdentifier
    , toFilePath
    ) where

import Control.Arrow (second)

import GHC.Exts (IsString, fromString)
import System.FilePath (joinPath)

-- | An identifier used to uniquely identify a value
--
newtype Identifier = Identifier {unIdentifier :: [String]}
                   deriving (Eq, Ord)

instance Show Identifier where
    show = toFilePath

instance IsString Identifier where
    fromString = parseIdentifier

-- | Parse an identifier from a string
--
parseIdentifier :: String -> Identifier
parseIdentifier = Identifier . filter (not . null) . split'
  where
    split' [] = [[]]
    split' str = let (pre, post) = second (drop 1) $ break (== '/') str
                 in pre : split' post

-- | Convert an identifier to a relative 'FilePath'
--
toFilePath :: Identifier -> FilePath
toFilePath = joinPath . unIdentifier
