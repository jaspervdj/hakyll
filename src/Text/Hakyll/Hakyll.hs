-- | Module describing the Hakyll monad stack.
module Text.Hakyll.Hakyll
    ( HakyllConfiguration (..)
    , Hakyll
    , askHakyll
    ) where

import Control.Monad.Reader (ReaderT, ask)
import Control.Monad (liftM)

import Text.Hakyll.Context (Context)

-- | Hakyll global configuration type.
data HakyllConfiguration = HakyllConfiguration
    { -- | An additional context to use when rendering. This additional context
      --   is used globally.
      additionalContext :: Context
    }

-- | Our custom monad stack.
type Hakyll = ReaderT HakyllConfiguration IO

-- | Simplified @ask@ function for the Hakyll monad stack.
askHakyll :: (HakyllConfiguration -> a) -> Hakyll a
askHakyll = flip liftM ask
