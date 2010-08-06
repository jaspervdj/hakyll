-- | Support for Hamlet templates in Hakyll.
--
module Text.Hakyll.Internal.Template.Hamlet
    ( isHamletRTFile
    , readHamletRT
    , fromHamletRT
    ) where

import Control.Exception (try)
import Control.Monad.Trans (liftIO)
import System.FilePath (takeExtension)

import Text.Hamlet.RT

import Text.Hakyll.Internal.Template.Template
import Text.Hakyll.HakyllMonad (Hakyll, askHakyll, hamletSettings, logHakyll)

-- | Determine if a file is a hamlet template by extension.
--
isHamletRTFile :: FilePath -> Bool
isHamletRTFile fileName = takeExtension fileName `elem` [".hamlet", ".hml"]

-- | Read a 'HamletRT' by file name.
--
readHamletRT :: FilePath         -- ^ Filename of the template
             -> Hakyll HamletRT  -- ^ Resulting hamlet template
readHamletRT fileName = do
    settings <- askHakyll hamletSettings
    string <- liftIO $ readFile fileName
    result <- liftIO $ try $ parseHamletRT settings string
    case result of
        Left (HamletParseException s) -> error' s
        Left (HamletUnsupportedDocException d) -> error' $ show d
        Left (HamletRenderException s) -> error' s
        Right x -> return x
  where
    error' s = do
        logHakyll $ "Parse of hamlet file " ++ fileName ++ " failed."
        logHakyll s
        error "Parse failed."

-- | Convert a 'HamletRT' to a 'Template'
--
fromHamletRT :: HamletRT  -- ^ Hamlet runtime template
             -> Template  -- ^ Hakyll template
fromHamletRT (HamletRT sd) = Template $ map fromSimpleDoc sd
  where
    fromSimpleDoc :: SimpleDoc -> TemplateElement
    fromSimpleDoc (SDRaw chunk) = Chunk chunk
    fromSimpleDoc (SDVar [var]) = Identifier var
    fromSimpleDoc (SDVar _) =
        error "Hakyll does not support '.' in identifier names when using \
              \hamlet templates."
    fromSimpleDoc _ =
        error "Only simple $key$ identifiers are allowed when using hamlet \
              \templates."
