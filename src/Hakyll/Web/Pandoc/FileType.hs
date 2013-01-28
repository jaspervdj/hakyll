--------------------------------------------------------------------------------
-- | A module dealing with pandoc file extensions and associated file types
module Hakyll.Web.Pandoc.FileType
    ( FileType (..)
    , fileType
    , itemFileType
    ) where


--------------------------------------------------------------------------------
import           System.FilePath        (takeExtension)


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item


--------------------------------------------------------------------------------
-- | Datatype to represent the different file types Hakyll can deal with by
-- default
data FileType
    = Binary
    | Css
    | Html
    | LaTeX
    | LiterateHaskell FileType
    | Markdown
    | OrgMode
    | PlainText
    | Rst
    | Textile
    deriving (Eq, Ord, Show, Read)


--------------------------------------------------------------------------------
-- | Get the file type for a certain file. The type is determined by extension.
fileType :: FilePath -> FileType
fileType = uncurry fileType' . splitExtension 
  where
    fileType' _ ".css"      = Css
    fileType' _ ".htm"      = Html
    fileType' _ ".html"     = Html
    fileType' f ".lhs"      = LiterateHaskell (fileType' (takeExtension f))
    fileType' _ ".markdown" = Markdown
    fileType' _ ".md"       = Markdown
    fileType' _ ".mdn"      = Markdown
    fileType' _ ".mdown"    = Markdown
    fileType' _ ".mdwn"     = Markdown
    fileType' _ ".mkd"      = Markdown
    fileType' _ ".mkdwn"    = Markdown
    fileType' _ ".org"      = OrgMode
    fileType' _ ".page"     = Markdown
    fileType' _ ".rst"      = Rst
    fileType' _ ".tex"      = LaTeX
    fileType' _ ".text"     = PlainText
    fileType' _ ".textile"  = Textile
    fileType' _ ".txt"      = PlainText
    fileType' _ _           = Binary  -- Treat unknown files as binary


--------------------------------------------------------------------------------
-- | Get the file type for the current file
itemFileType :: Item a -> FileType
itemFileType = fileType . toFilePath . itemIdentifier
