-- | Read templates in the hamlet format
--
{-# LANGUAGE MultiParamTypeClasses #-}
module Hakyll.Web.Template.Read.Hamlet
    ( readHamletTemplate
    , readHamletTemplateWith
    ) where

import Text.Hamlet (HamletSettings (..), defaultHamletSettings)
import Text.Hamlet.RT

import Hakyll.Web.Template.Internal

-- | Read a hamlet template using the default settings
--
readHamletTemplate :: String -> Template
readHamletTemplate = readHamletTemplateWith defaultHamletSettings

-- | Read a hamlet template using the specified settings
--
readHamletTemplateWith :: HamletSettings -> String -> Template
readHamletTemplateWith settings string =
    let result = parseHamletRT settings string
    in case result of
        Just hamlet -> fromHamletRT hamlet
        Nothing     -> error
            "Hakyll.Web.Template.Read.Hamlet.readHamletTemplateWith: \
            \Could not parse Hamlet file"

-- | Convert a 'HamletRT' to a 'Template'
--
fromHamletRT :: HamletRT  -- ^ Hamlet runtime template
             -> Template  -- ^ Hakyll template
fromHamletRT (HamletRT sd) = Template $ map fromSimpleDoc sd
  where
    fromSimpleDoc :: SimpleDoc -> TemplateElement
    fromSimpleDoc (SDRaw chunk) = Chunk chunk
    fromSimpleDoc (SDVar [var]) = Key var
    fromSimpleDoc (SDVar _) = error
        "Hakyll.Web.Template.Read.Hamlet.fromHamletRT: \
        \Hakyll does not support '.' in identifier names when using \
        \hamlet templates."
    fromSimpleDoc _ = error
        "Hakyll.Web.Template.Read.Hamlet.fromHamletRT: \
        \Only simple $key$ identifiers are allowed when using hamlet \
        \templates."
