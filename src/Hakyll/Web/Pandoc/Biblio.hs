--------------------------------------------------------------------------------
-- | Wraps pandocs bibiliography handling
--
-- In order to add a bibliography, you will need a bibliography file (e.g.
-- @.bib@) and a CSL file (@.csl@). Both need to be compiled with their
-- respective compilers ('biblioCompiler' and 'cslCompiler'). Then, you can
-- refer to these files when you use 'pageReadPandocBiblio'. This function also
-- takes the reader options for completeness -- you can use
-- 'defaultHakyllReaderOptions' if you're unsure.
{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Web.Pandoc.Biblio
    ( CSL
    , cslCompiler
    , Biblio (..)
    , biblioCompiler
    , readPandocBiblio
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Data.Binary            (Binary (..))
import           Data.Traversable       (traverse)
import           Data.Typeable          (Typeable)
import qualified Text.CSL               as CSL
import           Text.Pandoc            (Pandoc, ReaderOptions (..))
import           Text.Pandoc.Biblio     (processBiblio)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Writable
import           Hakyll.Web.Pandoc


--------------------------------------------------------------------------------
data CSL = CSL
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Binary CSL where
    put CSL = return ()
    get     = return CSL


--------------------------------------------------------------------------------
instance Writable CSL where
    -- Shouldn't be written.
    write _ _ = return ()


--------------------------------------------------------------------------------
cslCompiler :: Compiler (Item CSL)
cslCompiler = makeItem CSL


--------------------------------------------------------------------------------
newtype Biblio = Biblio [CSL.Reference]
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Binary Biblio where
    -- Ugly.
    get             = Biblio . read <$> get
    put (Biblio rs) = put $ show rs


--------------------------------------------------------------------------------
instance Writable Biblio where
    -- Shouldn't be written.
    write _ _ = return ()


--------------------------------------------------------------------------------
biblioCompiler :: Compiler (Item Biblio)
biblioCompiler = do
    filePath <- toFilePath <$> getUnderlying
    makeItem =<< unsafeCompiler (Biblio <$> CSL.readBiblioFile filePath)


--------------------------------------------------------------------------------
readPandocBiblio :: ReaderOptions
                 -> Maybe (Item CSL)
                 -> Item Biblio
                 -> (Item String)
                 -> Compiler (Item Pandoc)
readPandocBiblio ropt csl biblio item = do
    -- Parse CSL file, if given
    style <- unsafeCompiler $
        traverse (CSL.readCSLFile . toFilePath . itemIdentifier) csl

    -- We need to know the citation keys, add then *before* actually parsing the
    -- actual page. If we don't do this, pandoc won't even consider them
    -- citations!
    let Biblio refs = itemBody biblio
        ropt'       = ropt {readerReferences = readerReferences ropt ++ refs}
        pandoc      = itemBody $ readPandocWith ropt' item
        pandoc'     = processBiblio style refs pandoc

    return $ fmap (const pandoc') item
