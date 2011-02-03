-- | Exports a datastructure for the top-level hakyll configuration
--
module Hakyll.Core.Configuration
    ( HakyllConfiguration (..)
    , defaultHakyllConfiguration
    ) where

data HakyllConfiguration = HakyllConfiguration
    { -- | Directory in which the output written
      destinationDirectory :: FilePath
    , -- | Directory where hakyll's internal store is kept
      storeDirectory       :: FilePath
    } deriving (Show)

-- | Default configuration for a hakyll application
--
defaultHakyllConfiguration :: HakyllConfiguration
defaultHakyllConfiguration = HakyllConfiguration
    { destinationDirectory = "_site"
    , storeDirectory       = "_cache"
    }
