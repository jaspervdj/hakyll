import Text.Hakyll
import Text.Hakyll.Render
import Text.Hakyll.CreateContext
import Text.Hakyll.File
import Text.Hakyll.Regex
import Control.Monad.Reader (liftIO)
import System.Directory
import Control.Monad (mapM_, liftM)
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
    renderChain ["templates/tutorials.html", "templates/default.html"] $ withSidebar tutorialPage

    mapM_ render' $ [ "about.markdown"
                    , "index.markdown"
                    , "philosophy.markdown"
                    , "reference.markdown"
                    , "changelog.markdown"
                    ] ++ tutorials

  where
    render' = renderChain ["templates/default.html"] . withSidebar . createPage
    withSidebar a = a `combine` createPage "sidebar.markdown"
          
