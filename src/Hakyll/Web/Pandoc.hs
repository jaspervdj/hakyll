--------------------------------------------------------------------------------
-- | Module exporting convenient pandoc bindings
module Hakyll.Web.Pandoc
    ( -- * The basic building blocks
      readPandoc
    , readPandocWith
    , writePandoc
    , writePandocWith
    , renderPandoc
    , renderPandocWith

      -- * Derived compilers
    , pandocCompiler
    , pandocCompilerWith
    , pandocCompilerWithTransform

      -- * Default options
    , defaultHakyllParserState
    , defaultHakyllWriterOptions
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative        ((<$>))
import           Text.Pandoc


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Web.Pandoc.FileType


--------------------------------------------------------------------------------
-- | Read a string using pandoc, with the default options
readPandoc :: Item String  -- ^ String to read
           -> Item Pandoc  -- ^ Resulting document
readPandoc = readPandocWith defaultHakyllParserState


--------------------------------------------------------------------------------
-- | Read a string using pandoc, with the supplied options
readPandocWith :: ParserState  -- ^ Parser options
               -> Item String  -- ^ String to read
               -> Item Pandoc  -- ^ Resulting document
readPandocWith state item = fmap (reader state (itemFileType item)) item
  where
    reader s t = case t of
        Html               -> readHtml s
        LaTeX              -> readLaTeX s
        LiterateHaskell t' -> reader s {stateLiterateHaskell = True} t'
        Markdown           -> readMarkdown s
        Rst                -> readRST s
        Textile            -> readTextile s
        _                  -> error $
            "Hakyll.Web.readPandocWith: I don't know how to read a file of the " ++
            "type " ++ show t ++ " for: " ++ show (itemIdentifier item)


--------------------------------------------------------------------------------
-- | Write a document (as HTML) using pandoc, with the default options
writePandoc :: Item Pandoc  -- ^ Document to write
            -> Item String  -- ^ Resulting HTML
writePandoc = writePandocWith defaultHakyllWriterOptions


--------------------------------------------------------------------------------
-- | Write a document (as HTML) using pandoc, with the supplied options
writePandocWith :: WriterOptions  -- ^ Writer options for pandoc
                -> Item Pandoc    -- ^ Document to write
                -> Item String    -- ^ Resulting HTML
writePandocWith options = fmap $ writeHtmlString options


--------------------------------------------------------------------------------
-- | Render the resource using pandoc
renderPandoc :: Item String -> Item String
renderPandoc =
    renderPandocWith defaultHakyllParserState defaultHakyllWriterOptions


--------------------------------------------------------------------------------
-- | Render the resource using pandoc
renderPandocWith :: ParserState -> WriterOptions -> Item String -> Item String
renderPandocWith state options = writePandocWith options . readPandocWith state


--------------------------------------------------------------------------------
-- | Read a page render using pandoc
pandocCompiler :: Compiler (Item String)
pandocCompiler =
    pandocCompilerWith defaultHakyllParserState defaultHakyllWriterOptions


--------------------------------------------------------------------------------
-- | A version of 'pandocCompiler' which allows you to specify your own pandoc
-- options
pandocCompilerWith :: ParserState -> WriterOptions -> Compiler (Item String)
pandocCompilerWith state options = pandocCompilerWithTransform state options id


--------------------------------------------------------------------------------
-- | An extension of 'pandocCompilerWith' which allows you to specify a custom
-- pandoc transformation for the content
pandocCompilerWithTransform :: ParserState -> WriterOptions
                            -> (Pandoc -> Pandoc)
                            -> Compiler (Item String)
pandocCompilerWithTransform state options f = cached cacheName $
    writePandocWith options . fmap f . readPandocWith state <$> getResourceBody
  where
    cacheName = "Hakyll.Web.Page.pageCompilerWithPandoc"


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
