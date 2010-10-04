-- | Module exporting a pandoc arrow
--
module Text.Hakyll.Pandoc
    ( renderAction
    , renderActionWith
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Arrow (second, (>>>), arr, (&&&))

import Text.Pandoc

import Text.Hakyll.Internal.FileType
import Text.Hakyll.Page
import Text.Hakyll.HakyllMonad
import Text.Hakyll.HakyllAction
import Text.Hakyll.Context

-- | Get a render function for a given extension.
--
getRenderFunction :: HakyllAction FileType (String -> String)
getRenderFunction = createHakyllAction $ \fileType -> case fileType of
    Html -> return id
    Text -> return id
    UnknownFileType -> return id
    _ -> do parserState <- askHakyll pandocParserState
            writerOptions <- askHakyll pandocWriterOptions
            return $ writeHtmlString writerOptions
                   . readFunction fileType (readOptions parserState fileType)
  where
    readFunction ReStructuredText        = readRST
    readFunction LaTeX                   = readLaTeX
    readFunction Markdown                = readMarkdown
    readFunction LiterateHaskellMarkdown = readMarkdown
    readFunction t                       = error $ "Cannot render " ++ show t

    readOptions options LiterateHaskellMarkdown = options
        { stateLiterateHaskell = True }
    readOptions options _                       = options

-- | An action that renders the list of page sections to a context using pandoc
--
renderAction :: HakyllAction [PageSection] Context
renderAction = (arr id &&& (getFileType' >>> getRenderFunction))
             >>> renderActionWith
  where
    getFileType' = arr $ getFileType . fromMaybe "unknown" . lookup "path" 
                       . map (\(x, y, _) -> (x, y)) . map unPageSection

-- | An action to render pages, offering just a little more flexibility
--
renderActionWith :: HakyllAction ([PageSection], String -> String) Context
renderActionWith = createHakyllAction $ \(sections, render') -> return $
    Context $ M.fromList $ map (renderTriple render' . unPageSection) sections
  where
    renderTriple render' (k, v, r) = second (if r then render' else id) (k, v)
