-- | A page is a key-value mapping, representing a page on your site
--
-- A page is an important concept in Hakyll. It is a key-value mapping, and has
-- one field with an arbitrary type. A 'Page' thus consists of
--
-- * a key-value mapping (of the type @Map String String@);
--
-- * a value (of the type @a@).
--
-- Usually, the value will be a 'String' as well, and the value will be the body
-- of the page.
--
-- Pages can be constructed using Haskell, but they are usually parsed from a
-- file. The file format for pages is pretty straightforward.
--
-- > This is a simple page
-- > consisting of two lines.
--
-- This is a valid page with two lines. If we load this in Hakyll, there would
-- be no metadata, and the body would be the given text. Let's look at a page
-- with some metadata.
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
    , addDefaultFields
    , sortByBaseName
    ) where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow (arr, (>>^), (&&&), (>>>))
import System.FilePath (takeBaseName, takeDirectory)
import Data.Monoid (Monoid, mempty)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sortBy)
import Data.Ord (comparing)

import Hakyll.Core.Identifier
import Hakyll.Core.Compiler
import Hakyll.Core.ResourceProvider
import Hakyll.Web.Page.Internal
import Hakyll.Web.Page.Read
import Hakyll.Web.Page.Metadata
import Hakyll.Web.Pandoc
import Hakyll.Web.Template
import Hakyll.Web.Util.String

-- | Create a page from a body, without metadata
--
fromBody :: a -> Page a
fromBody = Page M.empty

-- | Create a metadata page, without a body
--
fromMap :: Monoid a => Map String String -> Page a
fromMap m = Page m mempty

-- | Convert a page to a map. The body will be placed in the @body@ key.
--
toMap :: Page String -> Map String String
toMap (Page m b) = M.insert "body" b m

-- | Read a page (do not render it)
--
readPageCompiler :: Compiler Resource (Page String)
readPageCompiler = getResourceString >>^ readPage

-- | Read a page, add default fields, substitute fields and render using pandoc
--
pageCompiler :: Compiler Resource (Page String)
pageCompiler = cached "Hakyll.Web.Page.pageCompiler" $
    readPageCompiler >>> addDefaultFields >>> arr applySelf >>> pageRenderPandoc

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
    addRoute (Just r) = setField "url" (toUrl r)

    -- Add title and category, based on identifier
    addIdentifier i = setField "title" (takeBaseName p)
                    . setField "category" (takeBaseName $ takeDirectory p)
                    . setField "path" p
      where
        p = toFilePath i

-- | Sort posts based on the basename of the post. This is equivalent to a
-- chronologival sort, because of the @year-month-day-title.extension@ naming
-- convention in Hakyll.
--
sortByBaseName :: [Page a] -> [Page a]
sortByBaseName = sortBy $ comparing $ takeBaseName . getField "path"
