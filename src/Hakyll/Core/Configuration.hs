-- | Exports a datastructure for the top-level hakyll configuration
--
module Hakyll.Core.Configuration
    ( HakyllConfiguration (..)
    , defaultHakyllConfiguration
    ) where

import System.FilePath (takeFileName)
import Data.List (isPrefixOf, isSuffixOf)

data HakyllConfiguration = HakyllConfiguration
    { -- | Directory in which the output written
      destinationDirectory :: FilePath
    , -- | Directory where hakyll's internal store is kept
      storeDirectory       :: FilePath
    , -- | Function to determine ignored files
      --
      -- In 'defaultHakyllConfiguration', the following files are ignored:
      --
      -- * files starting with a @.@
      --
      -- * files ending with a @~@
      --
      -- * files ending with @.swp@
      --
      ignoreFile           :: FilePath -> Bool
    }

-- | Default configuration for a hakyll application
--
defaultHakyllConfiguration :: HakyllConfiguration
defaultHakyllConfiguration = HakyllConfiguration
    { destinationDirectory = "_site"
    , storeDirectory       = "_cache"
    , ignoreFile           = ignoreFile'
    }
  where
    ignoreFile' path
        | "." `isPrefixOf` fileName = True
        | "~" `isSuffixOf` fileName = True
        | ".swp" `isSuffixOf` fileName = True
        | otherwise = False
      where
        fileName = takeFileName path
