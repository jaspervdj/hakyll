-- | A module dealing with common file extensions and associated file types.
--
module Hakyll.Web.FileType
    ( FileType (..)
    , fileType
    , getFileType
    ) where

import System.FilePath (takeExtension)
import Control.Applicative ((<$>))

import Hakyll.Core.Identifier
import Hakyll.Core.Target

-- | Datatype to represent the different file types Hakyll can deal with by
-- default
--
data FileType
    = Html
    | LaTeX
    | LiterateHaskell FileType
    | Markdown
    | ReStructuredText
    | PlainText
    | Css
    | UnknownFileType
    deriving (Eq, Ord, Show, Read)

-- | Get the file type for a certain file. The type is determined by extension.
--
fileType :: FilePath -> FileType
fileType = fileType' . takeExtension
  where
    fileType' ".htm"      = Html
    fileType' ".html"     = Html
    fileType' ".lhs"      = LiterateHaskell Markdown
    fileType' ".markdown" = Markdown
    fileType' ".md"       = Markdown
    fileType' ".mdn"      = Markdown
    fileType' ".mdown"    = Markdown
    fileType' ".mdwn"     = Markdown
    fileType' ".mkd"      = Markdown
    fileType' ".mkdwn"    = Markdown
    fileType' ".page"     = Markdown
    fileType' ".rst"      = ReStructuredText
    fileType' ".tex"      = LaTeX
    fileType' ".text"     = PlainText
    fileType' ".txt"      = PlainText
    fileType' ".css"      = Css
    fileType' _           = UnknownFileType

-- | Get the file type for the current file
--
getFileType :: TargetM a FileType
getFileType = fileType . toFilePath <$> getIdentifier
