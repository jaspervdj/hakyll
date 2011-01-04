-- | Module exporting commonly used web-related functions
--
module Hakyll.Web
    ( defaultPageRead
    , defaultTemplateRead
    , defaultRelativizeUrls
    ) where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow (arr, (>>>), (>>^), (&&&))

import Hakyll.Core.Compiler
import Hakyll.Web.Page
import Hakyll.Web.Pandoc
import Hakyll.Web.Template
import Hakyll.Web.RelativizeUrls
import Hakyll.Web.Util.String

defaultPageRead :: Compiler () (Page String)
defaultPageRead = cached "Hakyll.Web.defaultPageRead" $
    pageRead >>> addDefaultFields >>> arr applySelf >>> pageRenderPandoc

defaultRelativizeUrls :: Compiler (Page String) (Page String)
defaultRelativizeUrls = getRoute &&& id >>^ uncurry relativize
  where
    relativize Nothing = id
    relativize (Just r) = fmap (relativizeUrls $ toSiteRoot r)

defaultTemplateRead :: Compiler () Template
defaultTemplateRead = cached "Hakyll.Web.defaultTemplateRead" $
    getResourceString >>^ readTemplate
