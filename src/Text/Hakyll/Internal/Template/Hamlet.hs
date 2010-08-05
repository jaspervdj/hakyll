-- | Support for Hamlet templates in Hakyll.
--
module Text.Hakyll.Internal.Template.Hamlet
    ( isHamletRTFile
    , readHamletRT
    , fromHamletRT
    ) where

import Data.List (intercalate)
import Control.Monad.Trans (liftIO)
import System.FilePath (takeExtension)

import Text.Hamlet.RT

import Text.Hakyll.Internal.Template.Template
import Text.Hakyll.HakyllMonad (Hakyll, askHakyll, hamletSettings)

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
    liftIO $ parseHamletRT settings string 


-- | Convert a 'HamletRT' to a 'Template'
--
fromHamletRT :: HamletRT  -- ^ Hamlet runtime template
             -> Template  -- ^ Hakyll template
fromHamletRT (HamletRT sd) = fromSimpleDoc sd
  where
    fromSimpleDoc :: [SimpleDoc] -> Template
    fromSimpleDoc [] = End
    fromSimpleDoc (SDRaw chunk : xs) = Chunk chunk $ fromSimpleDoc xs
    fromSimpleDoc (SDVar vars : xs) =
        Identifier (intercalate "." vars) $ fromSimpleDoc xs
    fromSimpleDoc (_ : xs) = fromSimpleDoc xs  -- Unsupported elements
