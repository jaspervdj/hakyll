--------------------------------------------------------------------------------
-- | A page is a key-value mapping, representing a page on your site
--
-- A page is an important concept in Hakyll. It is a key-value mapping, and has
-- one field with an arbitrary type. A 'Page' thus consists of
--
-- * metadata (of the type @Map String String@);
--
-- * the actual value (of the type @a@).
--
-- Usually, the value will be a 'String' as well, and the value will be the body
-- of the page.
--
-- However, this is certainly no restriction. For example, @Page ByteString@
-- could be used to represent a binary item (e.g. an image) and some metadata.
--
-- Pages can be constructed using Haskell, but they are usually parsed from a
-- file. The file format for pages is pretty straightforward.
--
-- > This is a simple page
-- > consisting of two lines.
--
-- This is a valid page with two lines. If we load this in Hakyll, there would
-- be no metadata, and the body would be the given text. Let's look at a page
-- with some metadata:
--
-- > ---
-- > title: Alice's Adventures in Wonderland
-- > author: Lewis Caroll
-- > year: 1865
-- > ---
-- >
-- > Chapter I
-- > =========
-- >
-- > Down the Rabbit-Hole
-- > --------------------
-- >
-- > Alice was beginning to get very tired of sitting by her sister on the bank,
-- > and of having nothing to do: once or twice she had peeped into the book her
-- > sister was reading, but it had no pictures or conversations in it, "and
-- > what is the use of a book," thought Alice "without pictures or
-- > conversation?"
-- >
-- > ...
--
-- As you can see, we construct a metadata header in Hakyll using @---@. Then,
-- we simply list all @key: value@ pairs, and end with @---@ again. This page
-- contains three metadata fields and a body. The body is given in markdown
-- format, which can be easily rendered to HTML by Hakyll, using pandoc.
module Hakyll.Web.Page
    ( pageCompiler
    , pageCompilerWith
    , pageCompilerWithPandoc
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>))
import           Text.Pandoc          (Pandoc, ParserState, WriterOptions)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Web.Pandoc


--------------------------------------------------------------------------------
-- | Read a page render using pandoc
pageCompiler :: Compiler (Item String)
pageCompiler =
    pageCompilerWith defaultHakyllParserState defaultHakyllWriterOptions


--------------------------------------------------------------------------------
-- | A version of 'pageCompiler' which allows you to specify your own pandoc
-- options
pageCompilerWith :: ParserState -> WriterOptions -> Compiler (Item String)
pageCompilerWith state options = pageCompilerWithPandoc state options id


--------------------------------------------------------------------------------
-- | An extension of 'pageCompilerWith' which allows you to specify a custom
-- pandoc transformation for the content
pageCompilerWithPandoc :: ParserState -> WriterOptions
                       -> (Pandoc -> Pandoc)
                       -> Compiler (Item String)
pageCompilerWithPandoc state options f = cached cacheName $
    writePandocWith options . fmap f . readPandocWith state <$> getResourceBody
  where
    cacheName = "Hakyll.Web.Page.pageCompilerWithPandoc"
