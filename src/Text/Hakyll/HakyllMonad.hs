-- | Module describing the Hakyll monad stack.
module Text.Hakyll.HakyllMonad
    ( HakyllConfiguration (..)
    , PreviewMode (..)
    , Hakyll
    , askHakyll
    , getAdditionalContext
    ) where

import Control.Monad.Reader (ReaderT, ask)
import Control.Monad (liftM)
import qualified Data.Map as M

import Text.Pandoc (ParserState, WriterOptions)

import Text.Hakyll.Context (Context (..))

-- | Our custom monad stack.
--
type Hakyll = ReaderT HakyllConfiguration IO

-- | Preview mode.
--
data PreviewMode = BuildOnRequest
                 | BuildOnInterval
                 deriving (Show, Eq, Ord)

-- | Hakyll global configuration type.
--
data HakyllConfiguration = HakyllConfiguration
    { -- | Absolute URL of the site.
      absoluteUrl         :: String
    , -- | An additional context to use when rendering. This additional context
      --   is used globally.
      additionalContext   :: Context
    , -- | Directory where the site is placed.
      siteDirectory       :: FilePath
    , -- | Directory for cache files.
      cacheDirectory      :: FilePath
    , -- | Enable index links.
      enableIndexUrl      :: Bool
    , -- | The preview mode used
      previewMode         :: PreviewMode
    , -- | Pandoc parsing options
      pandocParserState   :: ParserState
    , -- | Pandoc writer options
      pandocWriterOptions :: WriterOptions
    }

-- | Simplified @ask@ function for the Hakyll monad stack.
--
--   Usage would typically be something like:
--
--   > doSomething :: a -> b -> Hakyll c
--   > doSomething arg1 arg2 = do
--   >     siteDirectory' <- askHakyll siteDirectory
--   >     ...
--
askHakyll :: (HakyllConfiguration -> a) -> Hakyll a
askHakyll = flip liftM ask

-- | Obtain the globally available, additional context.
--
getAdditionalContext :: HakyllConfiguration -> Context
getAdditionalContext configuration =
    let (Context c) = additionalContext configuration
    in Context $ M.insert "absolute" (absoluteUrl configuration) c
