-- | Module exporting a pandoc arrow
--
module Text.Hakyll.Pandoc
    ( renderAction
    , renderActionWith
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Arrow (second, (>>>), arr)

import Text.Pandoc

import Text.Hakyll.Internal.FileType
import Text.Hakyll.Page
import Text.Hakyll.HakyllMonad
import Text.Hakyll.HakyllAction
import Text.Hakyll.Context

-- | Get a render function for a given extension.
--
getRenderFunction :: FileType -> Hakyll (String -> String)
getRenderFunction Html = return id
getRenderFunction Text = return id
getRenderFunction UnknownFileType = return id
getRenderFunction fileType = do
    parserState <- askHakyll pandocParserState
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
renderAction = createHakyllAction $ \sections -> do
    let path = fromMaybe "unknown" $ lookup "path" 
                                   $ map (\(x, y, _) -> (x, y))
                                   $ map unPageSection sections
    render' <- getRenderFunction $ getFileType path
    runHakyllAction $ arr (const sections) >>> renderActionWith render'

-- | An action to render pages, offering just a little more flexibility
--
renderActionWith :: (String -> String) -> HakyllAction [PageSection] Context
renderActionWith render' = createHakyllAction $ \sections -> return $
    Context $ M.fromList $ map (renderTriple . unPageSection) sections
  where
    renderTriple (k, v, r) = second (if r then render' else id) (k, v)
