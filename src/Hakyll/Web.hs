-- | Module exporting commonly used web-related functions
--
module Hakyll.Web
    ( defaultApplyTemplate
    ) where

import Control.Arrow ((>>^))

import Hakyll.Core.Compiler
import Hakyll.Core.Writable
import Hakyll.Core.Identifier
import Hakyll.Core.ResourceProvider
import Hakyll.Web.Page
import Hakyll.Web.Template

defaultApplyTemplate :: Identifier                            -- ^ Template
                     -> Compiler (Page String) (Page String)  -- ^ Compiler
defaultApplyTemplate identifier = require identifier (flip applyTemplate)
