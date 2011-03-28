{-# LANGUAGE OverloadedStrings, Arrows #-}
module Main where

import Control.Monad (forM_)
import Control.Arrow (arr, (>>>))

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Compress CSS
    route   "css/*" idRoute
    compile "css/*" compressCssCompiler

    -- Render static pages
    forM_ ["about.markdown", "index.markdown", "products.markdown"] $ \p -> do
            route   p $ setExtension ".html"
            compile p $
                pageCompiler
                    >>> requireA "footer.markdown" (setFieldA "footer" $ arr pageBody)
                    >>> applyTemplateCompiler "templates/default.html"
                    >>> relativizeUrlsCompiler

    -- Compile footer
    compile "footer.markdown" pageCompiler

    -- Read templates
    compile "templates/*" templateCompiler
