-- | A module dealing with file extensions and associated file types.
module Text.Hakyll.Internal.FileType
    ( FileType (..)
    , getFileType
    , isRenderable
    , isRenderableFile
    ) where

import System.FilePath (takeExtension)

-- | Datatype to represent the different file types Hakyll can deal with.
data FileType = Html
              | LaTeX
              | LiterateHaskellMarkdown
              | Markdown
              | ReStructuredText
              | UnknownFileType
              deriving (Eq, Ord, Show, Read)

-- | Get the file type for a certain file. The type is determined by extension.
getFileType :: FilePath -> FileType
getFileType = getFileType' . takeExtension
  where
    getFileType' ".htm"      = Html
    getFileType' ".html"     = Html
    getFileType' ".lhs"      = LiterateHaskellMarkdown
    getFileType' ".markdown" = Markdown
    getFileType' ".md"       = Markdown
    getFileType' ".mdn"      = Markdown
    getFileType' ".mdown"    = Markdown
    getFileType' ".mdwn"     = Markdown
    getFileType' ".mkd"      = Markdown
    getFileType' ".mkdwn"    = Markdown
    getFileType' ".rst"      = ReStructuredText
    getFileType' ".tex"      = LaTeX
    getFileType' _           = UnknownFileType

-- | Check if a certain @FileType@ is renderable.
isRenderable :: FileType -> Bool
isRenderable UnknownFileType = False
isRenderable _               = True

-- | Check if a certain file is renderable.
isRenderableFile :: FilePath -> Bool
isRenderableFile = isRenderable . getFileType
