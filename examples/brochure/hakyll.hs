{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))
import Control.Monad (forM_)

import Hakyll

main :: IO ()
main = hakyll $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    forM_ ["about.rst", "index.markdown", "code.lhs"] $ \page ->
        match page $ do
            route   $ setExtension "html"
            compile $ pageCompiler
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler
