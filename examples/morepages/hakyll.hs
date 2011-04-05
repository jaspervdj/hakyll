{-# LANGUAGE OverloadedStrings, Arrows #-}
module Main where

import Control.Monad (forM_)
import Control.Arrow (arr, (>>>))

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Render static pages
    forM_ ["about.markdown", "index.markdown", "products.markdown"] $ \p ->
        match p $ do
            route   $ setExtension ".html"
            compile $ pageCompiler
                >>> requireA "footer.markdown" (setFieldA "footer" $ arr pageBody)
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    -- Compile footer
    match "footer.markdown" $ compile pageCompiler

    -- Read templates
    match "templates/*" $ compile templateCompiler
