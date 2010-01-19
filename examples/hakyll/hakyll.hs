import Text.Hakyll
import Text.Hakyll.Render
import Text.Hakyll.Renderables
import Text.Hakyll.File
import Text.Hakyll.Regex

import Control.Monad (mapM_, liftM)
import Data.List (sort)

main = hakyll defaultHakyllConfiguration $ do
    directory css "css"
    directory static "images"
    directory static "examples"
    directory static "reference"

    tutorials <- liftM (sort . filter (`matchesRegex` "tutorial[0-9]*.markdown")) $ getRecursiveContents "."
    let tutorialList = renderAndConcat ["templates/tutorialitem.html"]
                                       (map createPagePath tutorials)
        tutorialPage = createCustomPage "tutorials.html"
                                        ("templates/tutorialitem.html" : tutorials)
                                        [ ("title", Left "Tutorials")
                                        , ("tutorials", Right tutorialList)
                                        ]
    renderChain ["templates/tutorials.html", "templates/default.html"] $ withSidebar tutorialPage

    mapM_ render' $ [ "about.markdown"
                    , "index.markdown"
                    , "philosophy.markdown"
                    , "reference.markdown"
                    ] ++ tutorials

  where
    render' = renderChain ["templates/default.html"] . withSidebar . createPagePath
    withSidebar a = a `combine` createPagePath "sidebar.markdown"
          
