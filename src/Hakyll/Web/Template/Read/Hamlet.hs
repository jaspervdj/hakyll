-- | Read templates in the hamlet format
--
{-# LANGUAGE MultiParamTypeClasses #-}
module Hakyll.Web.Template.Read.Hamlet
    ( readHamletTemplate
    , readHamletTemplateWith
    ) where

import Text.Hamlet (HamletSettings, defaultHamletSettings)
import Text.Hamlet.RT
import Data.String

import Hakyll.Web.Template.Internal

-- | Read a hamlet template using the default settings
--
readHamletTemplate :: TemplateString a => String -> Template a
readHamletTemplate = readHamletTemplateWith defaultHamletSettings

-- | Read a hamlet template using the specified settings
--
readHamletTemplateWith :: TemplateString a => HamletSettings -> String -> Template a
readHamletTemplateWith settings string =
    let result = parseHamletRT settings string
    in case result of
        Just hamlet -> fromHamletRT hamlet
        Nothing     -> error
            "Hakyll.Web.Template.Read.Hamlet.readHamletTemplateWith: \
            \Could not parse Hamlet file"

-- | Convert a 'HamletRT' to a 'Template'
--
fromHamletRT :: TemplateString a => HamletRT   -- ^ Hamlet runtime template
                                 -> Template a -- ^ Hakyll template
fromHamletRT (HamletRT sd) = Template $ map fromSimpleDoc sd
  where
    fromSimpleDoc :: TemplateString a => SimpleDoc -> TemplateElement a
    fromSimpleDoc (SDRaw chunk) = Chunk (fromString chunk)
    fromSimpleDoc (SDVar [var]) = Key (fromString var)
    fromSimpleDoc (SDVar _) = error
        "Hakyll.Web.Template.Read.Hamlet.fromHamletRT: \
        \Hakyll does not support '.' in identifier names when using \
        \hamlet templates."
    fromSimpleDoc _ = error
        "Hakyll.Web.Template.Read.Hamlet.fromHamletRT: \
        \Only simple $key$ identifiers are allowed when using hamlet \
        \templates."
