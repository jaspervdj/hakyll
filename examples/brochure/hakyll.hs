{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))
import Control.Monad (forM_)

import Hakyll

main :: IO ()
main = hakyll $ do
    route   "css/*" idRoute
    compile "css/*" compressCssCompiler

    compile "templates/*" templateCompiler

    forM_ ["about.rst", "index.markdown", "code.lhs"] $ \page -> do
        route   page $ setExtension "html"
        compile page $ readPageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
