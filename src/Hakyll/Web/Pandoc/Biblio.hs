-- | Wraps pandocs bibiliography handling
--
-- In order to add a bibliography, you will need a bibliography file (e.g.
-- @.bib@) and a CSL file (@.csl@). Both need to be compiled with their
-- respective compilers ('biblioCompiler' and 'cslCompiler'). Then, you can
-- refer to these files when you use 'pageReadPandocBiblio'. This function also
-- takes a parser state for completeness -- you can use
-- 'defaultHakyllParserState' if you're unsure.
--
{-# LANGUAGE Arrows, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Hakyll.Web.Pandoc.Biblio
    ( CSL
    , cslCompiler
    , Biblio (..)
    , biblioCompiler
    , pageReadPandocBiblio
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (arr, returnA)
import Data.Typeable (Typeable)

import Data.Binary (Binary (..))
import Text.Pandoc (Pandoc, ParserState (..))
import Text.Pandoc.Biblio (processBiblio)
import qualified Text.CSL as CSL

import Hakyll.Core.Compiler
import Hakyll.Core.Identifier
import Hakyll.Core.Resource
import Hakyll.Core.Writable
import Hakyll.Web.Page
import Hakyll.Web.Pandoc

newtype CSL = CSL FilePath
    deriving (Binary, Show, Typeable, Writable)

cslCompiler :: Compiler Resource CSL
cslCompiler = arr (CSL . unResource)

newtype Biblio = Biblio [CSL.Reference]
    deriving (Show, Typeable)

instance Binary Biblio where
    -- Ugly.
    get             = Biblio . read <$> get
    put (Biblio rs) = put $ show rs

instance Writable Biblio where
    write _ _ = return ()

biblioCompiler :: Compiler Resource Biblio
biblioCompiler = unsafeCompiler $
    fmap Biblio . CSL.readBiblioFile . unResource

pageReadPandocBiblio :: ParserState
                     -> Identifier CSL
                     -> Identifier Biblio
                     -> Compiler (Page String) (Page Pandoc)
pageReadPandocBiblio state csl refs = proc page -> do
    CSL csl' <- require_ csl -< ()
    Biblio refs' <- require_ refs -< ()
    -- We need to know the citation keys, add then *before* actually parsing the
    -- actual page. If we don't do this, pandoc won't even consider them
    -- citations!
    let cits   = map CSL.refId refs'
        state' = state {stateCitations = stateCitations state ++ cits}
    pandocPage <- pageReadPandocWithA -< (state', page)
    let pandoc = pageBody pandocPage
    pandoc' <- unsafeCompiler processBiblio' -< (csl', refs', pandoc)
    returnA -< pandocPage {pageBody = pandoc'}
  where
    processBiblio' (c, r, p) = processBiblio c Nothing r p
