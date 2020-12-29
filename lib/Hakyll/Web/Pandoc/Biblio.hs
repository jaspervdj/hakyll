--------------------------------------------------------------------------------
-- | Wraps pandocs bibiliography handling
--
-- In order to add a bibliography, you will need a bibliography file (e.g.
-- @.bib@) and a CSL file (@.csl@). Both need to be compiled with their
-- respective compilers ('biblioCompiler' and 'cslCompiler'). Then, you can
-- refer to these files when you use 'readPandocBiblio'. This function also
-- takes the reader options for completeness -- you can use
-- 'defaultHakyllReaderOptions' if you're unsure.
-- 'pandocBiblioCompiler' is a convenience wrapper which works like 'pandocCompiler',
-- but also takes paths to compiled bibliography and csl files.
{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Hakyll.Web.Pandoc.Biblio
    ( CSL
    , cslCompiler
    , Biblio (..)
    , biblioCompiler
    , readPandocBiblio
    , pandocBiblioCompiler
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                 (liftM)
import           Data.Binary                   (Binary (..))
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Map                      as Map
import qualified Data.Time                     as Time
import           Data.Typeable                 (Typeable)
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Writable
import           Hakyll.Web.Pandoc
import           Text.Pandoc                   (Extension (..), Pandoc,
                                                ReaderOptions (..),
                                                enableExtension)
import qualified Text.Pandoc                   as Pandoc
import qualified Text.Pandoc.Citeproc          as Pandoc (processCitations)


--------------------------------------------------------------------------------
newtype CSL = CSL {unCSL :: B.ByteString}
    deriving (Binary, Show, Typeable)



--------------------------------------------------------------------------------
instance Writable CSL where
    -- Shouldn't be written.
    write _ _ = return ()


--------------------------------------------------------------------------------
cslCompiler :: Compiler (Item CSL)
cslCompiler = fmap (CSL . BL.toStrict) <$> getResourceLBS


--------------------------------------------------------------------------------
newtype Biblio = Biblio {unBiblio :: B.ByteString}
    deriving (Binary, Show, Typeable)


--------------------------------------------------------------------------------
instance Writable Biblio where
    -- Shouldn't be written.
    write _ _ = return ()


--------------------------------------------------------------------------------
biblioCompiler :: Compiler (Item Biblio)
biblioCompiler = fmap (Biblio . BL.toStrict) <$> getResourceLBS


--------------------------------------------------------------------------------
readPandocBiblio :: ReaderOptions
                 -> Item CSL
                 -> Item Biblio
                 -> (Item String)
                 -> Compiler (Item Pandoc)
readPandocBiblio ropt csl biblio item = do
    -- It's not straightforward to use the Pandoc API as of 2.11 to deal with
    -- citations, since it doesn't export many things in 'Text.Pandoc.Citeproc'.
    -- The 'citeproc' package is also hard to use.
    --
    -- So instead, we try treating Pandoc as a black box.  Pandoc can read
    -- specific csl and bilbio files based on metadata keys.
    --
    -- So we load the CSL and Biblio files and pass them to Pandoc using the
    -- ersatz filesystem.
    Pandoc.Pandoc (Pandoc.Meta meta) blocks <- itemBody <$>
        readPandocWith ropt item

    let cslFile = Pandoc.FileInfo zeroTime . unCSL $ itemBody csl
        bibFile = Pandoc.FileInfo zeroTime . unBiblio $ itemBody biblio
        addBiblioFiles = \st -> st
            { Pandoc.stFiles =
                Pandoc.insertInFileTree "_hakyll/style.csl" cslFile .
                Pandoc.insertInFileTree "_hakyll/refs.bib" bibFile $
                Pandoc.stFiles st
            }
        biblioMeta = Pandoc.Meta .
            Map.insert "csl" (Pandoc.MetaString "_hakyll/style.csl") .
            Map.insert "bibliography" (Pandoc.MetaString "_hakyll/refs.bib") $
            meta
        errOrPandoc = Pandoc.runPure $ do
            Pandoc.modifyPureState addBiblioFiles
            Pandoc.processCitations $ Pandoc.Pandoc biblioMeta blocks

    pandoc <- case errOrPandoc of
        Left  e -> compilerThrow ["Error during processCitations: " ++ show e]
        Right x -> return x

    return $ fmap (const pandoc) item

  where
    zeroTime = Time.UTCTime (toEnum 0) 0

--------------------------------------------------------------------------------
-- | Compiles a markdown file via Pandoc. Requires the .csl and .bib files to be known to the compiler via match statements.
pandocBiblioCompiler :: String -> String -> Compiler (Item String)
pandocBiblioCompiler cslFileName bibFileName = do
    csl <- load $ fromFilePath cslFileName
    bib <- load $ fromFilePath bibFileName
    liftM writePandoc
        (getResourceBody >>= readPandocBiblio ropt csl bib)
    where ropt = defaultHakyllReaderOptions
            { -- The following option enables citation rendering
              readerExtensions = enableExtension Ext_citations $ readerExtensions defaultHakyllReaderOptions
            }
