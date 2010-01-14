module Text.Hakyll.Hakyll
    ( HakyllConfiguration (..)
    , Hakyll
    ) where

import Text.Hakyll.Context (Context)
import System.FilePath (FilePath)
import Control.Monad.Reader (ReaderT)

data HakyllConfiguration = HakyllConfiguration
    { hakyllDestination :: FilePath
    , hakyllGlobalContext :: Context
    }

type Hakyll = ReaderT HakyllConfiguration IO
