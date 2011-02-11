module Hakyll.Web.Template
    ( Template
    , applyTemplate
    , applySelf
    , templateRead
    , templateReadWith
    ) where

import Control.Arrow
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import System.FilePath (takeExtension)

import Text.Hamlet (HamletSettings, defaultHamletSettings)

import Hakyll.Core.Compiler
import Hakyll.Core.Identifier
import Hakyll.Web.Template.Internal
import Hakyll.Web.Template.Read
import Hakyll.Web.Page

-- | Substitutes @$identifiers@ in the given @Template@ by values from the given
-- "Page". When a key is not found, it is left as it is. You can specify
-- the characters used to replace escaped dollars (@$$@) here.
--
applyTemplate :: Template -> Page String -> Page String
applyTemplate template page =
    fmap (const $ substitute =<< unTemplate template) page
  where
    substitute (Chunk chunk) = chunk
    substitute (Key key) =
        fromMaybe ('$' : key) $ M.lookup key $ toMap page
    substitute (Escaped) = "$"

-- | Apply a page as it's own template. This is often very useful to fill in
-- certain keys like @$root@ and @$url@.
--
applySelf :: Page String -> Page String
applySelf page = applyTemplate (readTemplate $ pageBody page) page

-- | Read a template. If the extension of the file we're compiling is
-- @.hml@ or @.hamlet@, it will be considered as a Hamlet template, and parsed
-- as such.
--
templateRead :: Compiler a Template
templateRead = templateReadWith defaultHamletSettings

-- | Version of 'templateRead' that enables custom settings.
--
templateReadWith :: HamletSettings -> Compiler a Template
templateReadWith settings =
    getIdentifier &&& getResourceString >>^ uncurry read'
  where
    read' identifier string =
        if takeExtension (toFilePath identifier) `elem` [".hml", ".hamlet"]
            -- Hamlet template
            then readHamletTemplateWith settings string
            -- Hakyll template
            else readTemplate string
