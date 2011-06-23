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
--
{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Web.Page
    ( Page (..)
    , fromBody
    , fromMap
    , toMap
    , readPageCompiler
    , pageCompiler
    , pageCompilerWith
    , pageCompilerWithPandoc
    , pageCompilerWithFields
    , addDefaultFields
    ) where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow (arr, (>>^), (&&&), (>>>))
import System.FilePath (takeBaseName, takeDirectory)
import qualified Data.Map as M

import Text.Pandoc (Pandoc, ParserState, WriterOptions)

import Hakyll.Core.Identifier
import Hakyll.Core.Compiler
import Hakyll.Core.Resource
import Hakyll.Web.Page.Internal
import Hakyll.Web.Page.Read
import Hakyll.Web.Page.Metadata
import Hakyll.Web.Pandoc
import Hakyll.Web.Template
import Hakyll.Web.Util.Url

-- | Create a page from a body, without metadata
--
fromBody :: a -> Page a
fromBody = Page M.empty

-- | Read a page (do not render it)
--
readPageCompiler :: Compiler Resource (Page String)
readPageCompiler = getResourceString >>^ readPage

-- | Read a page, add default fields, substitute fields and render using pandoc
--
pageCompiler :: Compiler Resource (Page String)
pageCompiler =
    pageCompilerWith defaultHakyllParserState defaultHakyllWriterOptions

-- | A version of 'pageCompiler' which allows you to specify your own pandoc
-- options
--
pageCompilerWith :: ParserState -> WriterOptions
                 -> Compiler Resource (Page String)
pageCompilerWith state options = pageCompilerWithPandoc state options id

-- | An extension of 'pageCompilerWith' which allows you to specify a custom
-- pandoc transformer for the content
--
pageCompilerWithPandoc :: ParserState -> WriterOptions
                       -> (Pandoc -> Pandoc)
                       -> Compiler Resource (Page String)
pageCompilerWithPandoc state options f = cached cacheName $
    readPageCompiler >>> addDefaultFields >>> arr applySelf
                     >>> pageReadPandocWith state
                     >>> arr (fmap (writePandocWith options . f))
  where
    cacheName = "Hakyll.Web.Page.pageCompilerWithPandoc"

-- | This is another, even more advanced version of 'pageCompilerWithPandoc'.
-- This function allows you to provide an arrow which is applied before the
-- fields in a page are rendered. This means you can use this extra customizable
-- stage to add custom fields which are inserted in the page.
--
pageCompilerWithFields :: ParserState -> WriterOptions
                       -> (Pandoc -> Pandoc)
                       -> Compiler (Page String) (Page String)
                       -> Compiler Resource (Page String)
pageCompilerWithFields state options f g =
    readPageCompiler >>> addDefaultFields >>> g >>> arr applySelf
                     >>> pageReadPandocWith state
                     >>> arr (fmap (writePandocWith options . f))

-- | Add a number of default metadata fields to a page. These fields include:
--
-- * @$url@
--
-- * @$category@
--
-- * @$title@
--
-- * @$path@
--
addDefaultFields :: Compiler (Page a) (Page a)
addDefaultFields =   (getRoute &&& id >>^ uncurry addRoute)
                 >>> (getIdentifier &&& id >>^ uncurry addIdentifier)
  where
    -- Add root and url, based on route
    addRoute Nothing  = id
    addRoute (Just r) = trySetField "url" (toUrl r)

    -- Add title and category, based on identifier
    addIdentifier i = trySetField "title" (takeBaseName p)
                    . trySetField "category" (takeBaseName $ takeDirectory p)
                    . trySetField "path" p
      where
        p = toFilePath i
