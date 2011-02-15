-- | Module exporting commonly used web-related functions
--
module Hakyll.Web
    ( defaultTemplateRead
    , defaultTemplateReadWith
    , defaultCopyFile
    , defaultApplyTemplate
    ) where

import Control.Arrow ((>>^))

import Text.Hamlet (HamletSettings)

import Hakyll.Core.Compiler
import Hakyll.Core.Writable
import Hakyll.Core.Identifier
import Hakyll.Core.ResourceProvider
import Hakyll.Web.Page
import Hakyll.Web.Template

defaultTemplateRead :: Compiler Resource Template
defaultTemplateRead = cached "Hakyll.Web.defaultTemplateRead" $ templateRead

defaultTemplateReadWith :: HamletSettings -> Compiler Resource Template
defaultTemplateReadWith settings = cached "Hakyll.Web.defaultTemplateReadWith" $
    templateReadWith settings

defaultCopyFile :: Compiler Resource CopyFile
defaultCopyFile = getIdentifier >>^ CopyFile . toFilePath

defaultApplyTemplate :: Identifier                            -- ^ Template
                     -> Compiler (Page String) (Page String)  -- ^ Compiler
defaultApplyTemplate identifier = require identifier (flip applyTemplate)
