--------------------------------------------------------------------------------
-- | Module exporting convenientpandoc bindings
module Hakyll.Web.Pandoc
    ( -- * The basic building blocks
      readPandoc
    , readPandocWith
    , writePandoc
    , writePandocWith

      -- * Functions working on pages/compilers
    , pageReadPandoc
    , pageReadPandocWith
    , pageRenderPandoc
    , pageRenderPandocWith

      -- * Default options
    , defaultHakyllParserState
    , defaultHakyllWriterOptions
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative        ((<$>))
import           Data.Maybe                 (fromMaybe)
import           Text.Pandoc


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Web.Page.Internal
import           Hakyll.Web.Pandoc.FileType


--------------------------------------------------------------------------------
-- | Read a string using pandoc, with the default options
readPandoc :: FileType          -- ^ Determines how parsing happens
           -> Maybe Identifier  -- ^ Optional, for better error messages
           -> Page              -- ^ String to read
           -> Pandoc            -- ^ Resulting document
readPandoc = readPandocWith defaultHakyllParserState


--------------------------------------------------------------------------------
-- | Read a string using pandoc, with the supplied options
readPandocWith :: ParserState       -- ^ Parser options
               -> FileType          -- ^ Determines parsing method
               -> Maybe Identifier  -- ^ Optional, for better error messages
               -> Page              -- ^ String to read
               -> Pandoc            -- ^ Resulting document
readPandocWith state fileType' id' = case fileType' of
    Html              -> readHtml state
    LaTeX             -> readLaTeX state
    LiterateHaskell t ->
        readPandocWith state {stateLiterateHaskell = True} t id'
    Markdown          -> readMarkdown state
    Rst               -> readRST state
    Textile           -> readTextile state
    t                 -> error $
        "Hakyll.Web.readPandocWith: I don't know how to read a file of the " ++
        "type " ++ show t ++ fromMaybe "" (fmap ((" for: " ++) . show) id')


--------------------------------------------------------------------------------
-- | Write a document (as HTML) using pandoc, with the default options
writePandoc :: Pandoc  -- ^ Document to write
            -> Page    -- ^ Resulting HTML
writePandoc = writePandocWith defaultHakyllWriterOptions


--------------------------------------------------------------------------------
-- | Write a document (as HTML) using pandoc, with the supplied options
writePandocWith :: WriterOptions  -- ^ Writer options for pandoc
                -> Pandoc         -- ^ Document to write
                -> Page           -- ^ Resulting HTML
writePandocWith = writeHtmlString


--------------------------------------------------------------------------------
-- | Read the resource using pandoc
pageReadPandoc :: Page -> Compiler Pandoc
pageReadPandoc = pageReadPandocWith defaultHakyllParserState


--------------------------------------------------------------------------------
-- | Read the resource using pandoc
pageReadPandocWith :: ParserState -> Page -> Compiler Pandoc
pageReadPandocWith state page = do
    identifier <- getIdentifier
    fileType'  <- getFileType
    return $ readPandocWith state fileType' (Just identifier) page


--------------------------------------------------------------------------------
-- | Render the resource using pandoc
pageRenderPandoc :: Page -> Compiler Page
pageRenderPandoc =
    pageRenderPandocWith defaultHakyllParserState defaultHakyllWriterOptions


--------------------------------------------------------------------------------
-- | Render the resource using pandoc
pageRenderPandocWith :: ParserState -> WriterOptions -> Page -> Compiler Page
pageRenderPandocWith state options page =
    writePandocWith options <$> pageReadPandocWith state page


--------------------------------------------------------------------------------
-- | The default reader options for pandoc parsing in hakyll
defaultHakyllParserState :: ParserState
defaultHakyllParserState = defaultParserState
    { -- The following option causes pandoc to read smart typography, a nice
      -- and free bonus.
      stateSmart = True
    }


--------------------------------------------------------------------------------
-- | The default writer options for pandoc rendering in hakyll
defaultHakyllWriterOptions :: WriterOptions
defaultHakyllWriterOptions = defaultWriterOptions
    { -- This option causes literate haskell to be written using '>' marks in
      -- html, which I think is a good default.
      writerLiterateHaskell = True
    }
