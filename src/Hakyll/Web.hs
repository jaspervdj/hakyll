-- | Module exporting commonly used web-related functions
--
module Hakyll.Web
    ( defaultPageRead
    , defaultTemplateRead
    ) where

import Control.Arrow (arr, (>>>), (>>^))

import Hakyll.Core.Compiler
import Hakyll.Web.Page
import Hakyll.Web.Pandoc
import Hakyll.Web.Template

defaultPageRead :: Compiler () (Page String)
defaultPageRead = cached "Hakyll.Web.defaultPageRead" $
    pageRead >>> addDefaultFields >>> arr applySelf >>> pageRenderPandoc

defaultTemplateRead :: Compiler () Template
defaultTemplateRead = cached "Hakyll.Web.defaultTemplateRead" $
    getResourceString >>^ readTemplate
