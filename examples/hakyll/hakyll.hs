import Text.Hakyll
import Text.Hakyll.Render
import Text.Hakyll.Renderables
import Text.Hakyll.File

import Control.Monad (mapM_, liftM)
import Data.List (sort)

import Text.Regex.Posix

main = hakyll $ do
    directory css "css"
    directory static "images"
    directory static "examples"

    tutorials <- liftM (sort . filter (=~ "tutorial[0-9]*.markdown")) $ getRecursiveContents "."
    let tutorialList = renderAndConcat "templates/tutorialitem.html"
                                       (map createPagePath tutorials)
        tutorialPage = createCustomPage "tutorials.html"
                                        ("templates/tutorialitem.html" : tutorials)
                                        [ ("title", Left "Tutorials")
                                        , ("tutorials", Right tutorialList)
                                        ]
    renderChain ["templates/tutorials.html", "templates/default.html"] tutorialPage

    mapM_ render' $ [ "about.markdown"
                    , "index.markdown"
                    ] ++ tutorials

    where render' = renderChain ["templates/default.html"] . createPagePath
