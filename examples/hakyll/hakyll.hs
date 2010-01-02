import Text.Hakyll
import Text.Hakyll.Render
import Text.Hakyll.Renderables
import Text.Hakyll.File

import Control.Monad(mapM_)

main = hakyll $ do
    directory css "css"
    directory static "images"
    directory static "examples"

    mapM_ render' [ "about.markdown"
                  , "index.markdown"
                  , "tutorials.markdown"
                  , "tutorial1.markdown"
                  , "tutorial2.markdown"
                  , "tutorial3.markdown"
                  ]

    where render' = renderChain ["templates/default.html"] . createPagePath
