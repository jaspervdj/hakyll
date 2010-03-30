import Text.Hakyll
import Text.Hakyll.Render
import Text.Hakyll.CreateContext
import Text.Hakyll.File
import Text.Hakyll.Regex
import Control.Monad.Reader (liftIO)
import System.Directory
import Control.Monad (mapM_, forM_, liftM)
import Data.List (sort)

main = hakyll "http://jaspervdj.be/hakyll" $ do
    directory css "css"
    directory static "images"
    directory static "examples"
    directory static "reference"

    tutorials <- liftM sort $ getRecursiveContents "tutorials"
    let tutorialPage = createListing "tutorials.html"
                                     ["templates/tutorialitem.html"]
                                     (map createPage tutorials)
                                     [("title", Left "Tutorials")]
    renderChain ["templates/tutorials.html", "templates/default.html"]
                (withSidebar tutorialPage)

    mapM_ (render' ["templates/default.html"]) $ 
        [ "about.markdown"
        , "index.markdown"
        , "philosophy.markdown"
        , "reference.markdown"
        , "changelog.markdown"
        ] 

    forM_ tutorials $ render' [ "templates/tutorial.html"
                              , "templates/default.html"
                              ] 

  where
    render' templates = renderChain templates . withSidebar . createPage
    withSidebar a = a `combine` createPage "sidebar.markdown"
          
