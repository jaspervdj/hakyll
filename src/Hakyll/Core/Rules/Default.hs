{-# LANGUAGE OverloadedStrings          #-}
module Hakyll.Core.Rules.Default 
    ( internalRules
    , addMetadataDependencies 
    )
where
import           Hakyll.Core.Rules
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal (compilerTellDependencies)
import           Hakyll.Core.Metadata          (getMatches, metadataFiles)
import           Hakyll.Core.Identifier.Pattern(fromList)

internalRules :: Rules ()
internalRules  = do
    match "metadata" $ compile $ makeItem ()
    match "**/metadata" $ compile $ makeItem ()

--------------------------------------------------------------------------------
addMetadataDependencies :: Compiler ()
addMetadataDependencies =
    compilerTellDependencies . map IdentifierDependency =<< getMatches . fromList =<< fmap metadataFiles getUnderlying


