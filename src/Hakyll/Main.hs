-- | Module providing the main hakyll function and command-line argument parsing
--
module Hakyll.Main
    ( hakyll
    , hakyllWith
    ) where

import Hakyll.Core.Configuration
import Hakyll.Core.Run
import Hakyll.Core.Rules

-- | This usualy is the function with which the user runs the hakyll compiler
--
hakyll :: Rules -> IO ()
hakyll = run defaultHakyllConfiguration

-- | A variant of 'hakyll' which allows the user to specify a custom
-- configuration
--
hakyllWith :: HakyllConfiguration -> Rules -> IO ()
hakyllWith = run
