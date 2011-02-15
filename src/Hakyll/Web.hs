-- | Module exporting commonly used web-related functions
--
module Hakyll.Web
    ( defaultTemplateRead
    , defaultTemplateReadWith
    , defaultRelativizeUrls
    , defaultCopyFile
    , defaultApplyTemplate
    ) where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>^), (&&&))

import Text.Hamlet (HamletSettings)

import Hakyll.Core.Compiler
import Hakyll.Core.Writable
import Hakyll.Core.Identifier
import Hakyll.Core.ResourceProvider
import Hakyll.Web.Page
import Hakyll.Web.Template
import Hakyll.Web.RelativizeUrls
import Hakyll.Web.Util.String

defaultRelativizeUrls :: Compiler (Page String) (Page String)
defaultRelativizeUrls = getRoute &&& id >>^ uncurry relativize
  where
    relativize Nothing = id
    relativize (Just r) = fmap (relativizeUrls $ toSiteRoot r)

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
