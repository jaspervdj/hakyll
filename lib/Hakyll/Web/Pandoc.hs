--------------------------------------------------------------------------------
-- | Module exporting convenient pandoc bindings
module Hakyll.Web.Pandoc
    ( -- * The basic building blocks
      readPandoc
    , readPandocWith
    , readPandocLBS
    , readPandocLBSWith
    , writePandoc
    , writePandocWith
    , renderPandoc
    , renderPandocWith

      -- * Derived compilers
    , pandocCompiler
    , pandocCompilerWith
    , pandocCompilerWithTransform
    , pandocCompilerWithTransformM

      -- * Default options
    , defaultHakyllReaderOptions
    , defaultHakyllWriterOptions
    ) where


--------------------------------------------------------------------------------
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Text.Pandoc
--import           Text.Pandoc.App.FormatHeuristics (formatFromFilePaths)
import           Text.Pandoc.Highlighting   (pygments)
import           Data.Char                  (toLower)
import           Data.Bifunctor             (second)
import           System.FilePath            (takeExtension)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
-- import           Hakyll.Web.Pandoc.FileType


--------------------------------------------------------------------------------
-- | Determine format based on file extension.
-- Adapted from Pandoc, pending the public importability of
-- Text.Pandoc.App.FormatHeuristics.
formatFromFilePath :: String -> FilePath -> String
formatFromFilePath fallback x =
  case takeExtension (map toLower x) of
    ".adoc"     -> "asciidoc"
    ".asciidoc" -> "asciidoc"
    ".context"  -> "context"
    ".ctx"      -> "context"
    ".db"       -> "docbook"
    ".doc"      -> "doc"  -- so we get an "unknown reader" error
    ".docx"     -> "docx"
    ".dokuwiki" -> "dokuwiki"
    ".epub"     -> "epub"
    ".fb2"      -> "fb2"
    ".htm"      -> "html"
    ".html"     -> "html"
    ".icml"     -> "icml"
    ".json"     -> "json"
    ".latex"    -> "latex"
    ".lhs"      -> "markdown+lhs"
    ".ltx"      -> "latex"
    ".markdown" -> "markdown"
    ".md"       -> "markdown"
    ".ms"       -> "ms"
    ".muse"     -> "muse"
    ".native"   -> "native"
    ".odt"      -> "odt"
    ".opml"     -> "opml"
    ".org"      -> "org"
    ".pdf"      -> "pdf"  -- so we get an "unknown reader" error
    ".pptx"     -> "pptx"
    ".roff"     -> "ms"
    ".rst"      -> "rst"
    ".rtf"      -> "rtf"
    ".s5"       -> "s5"
    ".t2t"      -> "t2t"
    ".tei"      -> "tei"
    ".tei.xml"  -> "tei"
    ".tex"      -> "latex"
    ".texi"     -> "texinfo"
    ".texinfo"  -> "texinfo"
    ".text"     -> "markdown"
    ".textile"  -> "textile"
    ".txt"      -> "markdown"
    ".wiki"     -> "mediawiki"
    ".xhtml"    -> "html"
    ".ipynb"    -> "ipynb"
    ['.',y]     | y `elem` ['1'..'9'] -> "man"
    _           -> fallback


--------------------------------------------------------------------------------
defaultReaderName :: Identifier -> String
defaultReaderName = formatFromFilePath "markdown" . toFilePath


--------------------------------------------------------------------------------
getReaderForIdentifier :: Identifier
                       -> (String, Reader PandocPure, Extensions)
getReaderForIdentifier ident =
    let readerName = defaultReaderName ident in
    case getReader readerName of
        Left _ -> error $
            "Hakyll.Web.readPandocWith: I don't know how to read a file of " ++
            "the type " ++ show readerName ++ " for: " ++ show ident
        Right (x, e) -> (readerName, x, e)


--------------------------------------------------------------------------------
-- | Read a string using pandoc, with the default options
readPandoc
    :: Item String             -- ^ String to read
    -> Compiler (Item Pandoc)  -- ^ Resulting document
readPandoc = readPandocWith defaultHakyllReaderOptions


--------------------------------------------------------------------------------
-- | Read a bytestring using pandoc, with the default options
readPandocLBS
    :: Item BL.ByteString             -- ^ String to read
    -> Compiler (Item Pandoc)  -- ^ Resulting document
readPandocLBS = readPandocLBSWith defaultHakyllReaderOptions


--------------------------------------------------------------------------------
-- | Read a string using pandoc, with the supplied options
readPandocWith
    :: ReaderOptions           -- ^ Parser options
    -> Item String             -- ^ String to read
    -> Compiler (Item Pandoc)  -- ^ Resulting document
readPandocWith ropt item =
    case runPure $ traverse (reader ropt (itemIdentifier item)) (T.pack <$> item) of
        Left err    -> fail $
            "Hakyll.Web.Pandoc.readPandocWith: parse failed: " ++ show err
        Right item' -> return item'
  where
    reader ro i = case getReaderForIdentifier i of
        (_, TextReader r, es) -> r (addExts ro es)
        (ext, ByteStringReader _, _) -> error $
            "Hakyll.Web.readPandocWith: files of the type " ++ (show ext) ++
            " must be read by the ByteString-capable reader, for: " ++ (show i)
            -- graceful degradation is impossible, as the UTF8 encoding mangles
            -- any invalid characters; at best, could try rereading the file?

    addExts ro es = ro {readerExtensions = es <> (readerExtensions ro)}

--------------------------------------------------------------------------------
-- | Read a bytestring using pandoc, with the supplied options; gracefully
-- decodes the bytestring into a String whenever accidentally used for a
-- textual input format
readPandocLBSWith
    :: ReaderOptions           -- ^ Parser options
    -> Item BL.ByteString      -- ^ ByteString to read
    -> Compiler (Item Pandoc)  -- ^ Resulting document
readPandocLBSWith ropt item =
    case runPure $ traverse (reader ropt (itemIdentifier item)) item of
        Left err    -> fail $
            "Hakyll.Web.Pandoc.readPandocLBSWith: parse failed: " ++ show err
        Right item' -> return item'
  where
    reader ro i = case getReaderForIdentifier i of
        (_, TextReader r, es) -> r (addExts ro es) . TL.toStrict . TLE.decodeUtf8
        (_, ByteStringReader r, es) -> r (addExts ro es)

    addExts ro es = ro {readerExtensions = es <> (readerExtensions ro)}


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
writePandocWith wopt (Item itemi doc) =
    case runPure $ writeHtml5String wopt doc of
        Left err    -> error $ "Hakyll.Web.Pandoc.writePandocWith: " ++ show err
        Right item' -> Item itemi $ T.unpack item'


--------------------------------------------------------------------------------
-- | Render the resource using pandoc
renderPandoc :: Item String -> Compiler (Item String)
renderPandoc =
    renderPandocWith defaultHakyllReaderOptions defaultHakyllWriterOptions


--------------------------------------------------------------------------------
-- | Render the resource using pandoc
renderPandocWith
    :: ReaderOptions -> WriterOptions -> Item String -> Compiler (Item String)
renderPandocWith ropt wopt item =
    writePandocWith wopt <$> readPandocWith ropt item


--------------------------------------------------------------------------------
-- | Read a page render using pandoc
pandocCompiler :: Compiler (Item String)
pandocCompiler =
    pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions


--------------------------------------------------------------------------------
-- | A version of 'pandocCompiler' which allows you to specify your own pandoc
-- options
pandocCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
pandocCompilerWith ropt wopt =
    cached "Hakyll.Web.Pandoc.pandocCompilerWith" $
        pandocCompilerWithTransform ropt wopt id


--------------------------------------------------------------------------------
-- | An extension of 'pandocCompilerWith' which allows you to specify a custom
-- pandoc transformation for the content
pandocCompilerWithTransform :: ReaderOptions -> WriterOptions
                            -> (Pandoc -> Pandoc)
                            -> Compiler (Item String)
pandocCompilerWithTransform ropt wopt f =
    pandocCompilerWithTransformM ropt wopt (return . f)


--------------------------------------------------------------------------------
-- | Similar to 'pandocCompilerWithTransform', but the transformation
-- function is monadic. This is useful when you want the pandoc
-- transformation to use the 'Compiler' information such as routes,
-- metadata, etc
pandocCompilerWithTransformM :: ReaderOptions -> WriterOptions
                    -> (Pandoc -> Compiler Pandoc)
                    -> Compiler (Item String)
pandocCompilerWithTransformM ropt wopt f =
    writePandocWith wopt <$>
        (traverse f =<< useUnderlyingReader)
  where
      getUnderlyingReader :: Compiler (Either String (Reader PandocPure, Extensions))
      getUnderlyingReader = getReader . defaultReaderName <$> getUnderlying
      addExts ro es = ro {readerExtensions = es <> (readerExtensions ro)}
      innerRead :: Either String (Reader PandocPure, Extensions) -> Compiler (Item Pandoc)
      innerRead (Left _) =
          readPandocLBSWith ropt =<< getResourceLBS
      innerRead (Right (ByteStringReader _, es)) =
          readPandocLBSWith (addExts ropt es) =<< getResourceLBS
      innerRead (Right (TextReader _, es)) =
          readPandocWith (addExts ropt es) =<< getResourceBody
      useUnderlyingReader :: Compiler (Item Pandoc)
      useUnderlyingReader = innerRead =<< getUnderlyingReader


--------------------------------------------------------------------------------
-- | The default reader options for pandoc parsing in hakyll
defaultHakyllReaderOptions :: ReaderOptions
defaultHakyllReaderOptions = def
    { -- The following option causes pandoc to read smart typography, a nice
      -- and free bonus.
      readerExtensions = enableExtension Ext_smart pandocExtensions
    }


--------------------------------------------------------------------------------
-- | The default writer options for pandoc rendering in hakyll
defaultHakyllWriterOptions :: WriterOptions
defaultHakyllWriterOptions = def
    { -- This option causes literate haskell to be written using '>' marks in
      -- html, which I think is a good default.
      writerExtensions = enableExtension Ext_smart pandocExtensions
    , -- We want to have hightlighting by default, to be compatible with earlier
      -- Hakyll releases
      writerHighlightStyle = Just pygments
    }
