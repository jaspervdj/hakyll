-- | Module exporting pandoc bindings
--
module Hakyll.Web.Pandoc
    ( -- * The basic building blocks
      readPandoc
    , readPandocWith
    , writePandoc
    , writePandocWith

      -- * Functions working on pages/compilers
    , pageReadPandoc
    , pageReadPandocWith
    , pageReadPandocWithA
    , pageRenderPandoc
    , pageRenderPandocWith

      -- * Default options
    , defaultHakyllParserState
    , defaultHakyllWriterOptions
    ) where

import Prelude hiding (id)
import Control.Applicative ((<$>))
import Control.Arrow ((>>>), (>>^), (&&&), (***))
import Control.Category (id)
import Data.Maybe (fromMaybe)

import Text.Pandoc

import Hakyll.Core.Compiler
import Hakyll.Core.Identifier
import Hakyll.Core.Util.Arrow
import Hakyll.Web.Pandoc.FileType
import Hakyll.Web.Page.Internal

-- | Read a string using pandoc, with the default options
--
readPandoc :: FileType              -- ^ Determines how parsing happens
           -> Maybe (Identifier a)  -- ^ Optional, for better error messages
           -> String                -- ^ String to read
           -> Pandoc                -- ^ Resulting document
readPandoc = readPandocWith defaultHakyllParserState

-- | Read a string using pandoc, with the supplied options
--
readPandocWith :: ParserState           -- ^ Parser options
               -> FileType              -- ^ Determines parsing method
               -> Maybe (Identifier a)  -- ^ Optional, for better error messages
               -> String                -- ^ String to read
               -> Pandoc                -- ^ Resulting document
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

-- | Write a document (as HTML) using pandoc, with the default options
--
writePandoc :: Pandoc  -- ^ Document to write
            -> String  -- ^ Resulting HTML
writePandoc = writePandocWith defaultHakyllWriterOptions

-- | Write a document (as HTML) using pandoc, with the supplied options
--
writePandocWith :: WriterOptions  -- ^ Writer options for pandoc
                -> Pandoc         -- ^ Document to write
                -> String         -- ^ Resulting HTML
writePandocWith = writeHtmlString

-- | Read the resource using pandoc
--
pageReadPandoc :: Compiler (Page String) (Page Pandoc)
pageReadPandoc = pageReadPandocWith defaultHakyllParserState

-- | Read the resource using pandoc
--
pageReadPandocWith :: ParserState -> Compiler (Page String) (Page Pandoc)
pageReadPandocWith state = constA state &&& id >>> pageReadPandocWithA

-- | Read the resource using pandoc. This is a (rarely needed) variant, which
-- comes in very useful when the parser state is the result of some arrow.
--
pageReadPandocWithA :: Compiler (ParserState, Page String) (Page Pandoc)
pageReadPandocWithA =
    id *** id &&& getIdentifier &&& getFileType >>^ pageReadPandocWithA'
  where
    pageReadPandocWithA' (s, (p, (i, t))) = readPandocWith s t (Just i) <$> p

-- | Render the resource using pandoc
--
pageRenderPandoc :: Compiler (Page String) (Page String)
pageRenderPandoc =
    pageRenderPandocWith defaultHakyllParserState defaultHakyllWriterOptions

-- | Render the resource using pandoc
--
pageRenderPandocWith :: ParserState
                     -> WriterOptions
                     -> Compiler (Page String) (Page String)
pageRenderPandocWith state options =
    pageReadPandocWith state >>^ fmap (writePandocWith options)

-- | The default reader options for pandoc parsing in hakyll
--
defaultHakyllParserState :: ParserState
defaultHakyllParserState = defaultParserState
    { -- The following option causes pandoc to read smart typography, a nice
      -- and free bonus.
      stateSmart = True
    }

-- | The default writer options for pandoc rendering in hakyll
--
defaultHakyllWriterOptions :: WriterOptions
defaultHakyllWriterOptions = defaultWriterOptions
    { -- This option causes literate haskell to be written using '>' marks in
      -- html, which I think is a good default.
      writerLiterateHaskell = True
    }
