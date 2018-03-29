{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))

import Hakyll ( Configuration (..)
              , applyAsTemplate
              , compile
              , compressCssCompiler
              , constField
              , copyFileCompiler
              , defaultConfiguration
              , defaultContext
              , getResourceBody
              , hakyllWith
              , idRoute
              , loadAndApplyTemplate
              , match
              , relativizeUrls
              , route
              , templateBodyCompiler
              )

customConfiguration :: Configuration
customConfiguration = defaultConfiguration
    { destinationDirectory = "output/"
    , providerDirectory    = "src/"
    , storeDirectory       = "chunks/store/"
    , tmpDirectory         = "chunks/tmp/"
    }

main :: IO ()
main = hakyllWith customConfiguration $ do
    match "index.html" $ do
        route idRoute
        compile $ do
            let context = constField "title" "Your Title"
                      <> constField "name" "Your Name"
                      <> constField "bio" "Your bio."
                      <> defaultContext
            getResourceBody
                >>= applyAsTemplate context
                >>= loadAndApplyTemplate "templates/default.html" context
                >>= relativizeUrls

    match "assets/css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "assets/img/*" $ do
        route idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler
