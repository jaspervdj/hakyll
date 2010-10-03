-- | Module exporting a pandoc arrow
--
module Text.Hakyll.Pandoc
    ( renderAction
    ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Arrow (second)

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

-- | Path must be there
--
renderAction :: HakyllAction [PageSection] Context
renderAction = createHakyllAction $ \sections -> do
    let triples = unPageSection =<< sections
        path = fromMaybe "unknown" $ lookup "path" 
                                   $ map (\(x, y, _) -> (x, y))
                                   $ triples
    render' <- getRenderFunction $ getFileType path
    let pairs = map (\(k, v, r) -> second (if r then render' else id) (k, v))
                    triples
    return $ Context $ M.fromList pairs
