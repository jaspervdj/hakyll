--------------------------------------------------------------------------------
-- | Wraps pandocs bibiliography handling
--
-- In order to add a bibliography, you will need a bibliography file (e.g.
-- @.bib@) and a CSL file (@.csl@). Both need to be compiled with their
-- respective compilers ('biblioCompiler' and 'cslCompiler'). Then, you can
-- refer to these files when you use 'pageReadPandocBiblio'. This function also
-- takes a parser state for completeness -- you can use
-- 'defaultHakyllParserState' if you're unsure.
{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Web.Pandoc.Biblio
    ( CSL
    , cslCompiler
    , Biblio (..)
    , biblioCompiler
    , pageReadPandocBiblio
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Data.Binary            (Binary (..))
import           Data.Typeable          (Typeable)
import qualified Text.CSL               as CSL
import           Text.Pandoc            (Pandoc, ParserState (..))
import           Text.Pandoc.Biblio     (processBiblio)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Writable
import           Hakyll.Web.Page
import           Hakyll.Web.Pandoc


--------------------------------------------------------------------------------
newtype CSL = CSL FilePath
    deriving (Binary, Show, Typeable, Writable)


--------------------------------------------------------------------------------
cslCompiler :: Compiler CSL
cslCompiler = CSL . toFilePath <$> getIdentifier


--------------------------------------------------------------------------------
newtype Biblio = Biblio [CSL.Reference]
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Binary Biblio where
    -- Ugly.
    get             = Biblio . read <$> get
    put (Biblio rs) = put $ show rs

instance Writable Biblio where
    write _ _ = return ()


--------------------------------------------------------------------------------
biblioCompiler :: Compiler Biblio
biblioCompiler = do
    filePath <- toFilePath <$> getIdentifier
    unsafeCompiler $ Biblio <$> CSL.readBiblioFile filePath


--------------------------------------------------------------------------------
pageReadPandocBiblio :: ParserState
                     -> CSL
                     -> Biblio
                     -> Page
                     -> Compiler Pandoc
pageReadPandocBiblio state (CSL csl) (Biblio refs) page = do
    -- We need to know the citation keys, add then *before* actually parsing the
    -- actual page. If we don't do this, pandoc won't even consider them
    -- citations!
    let cits   = map CSL.refId refs
        state' = state {stateCitations = stateCitations state ++ cits}
    pandoc  <- pageReadPandocWith state' page
    pandoc' <- unsafeCompiler $ processBiblio csl Nothing refs pandoc
    return pandoc'
