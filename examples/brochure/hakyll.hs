import Text.Hakyll (hakyll)
import Text.Hakyll.File (directory)
import Text.Hakyll.Render (css, static, renderChain)
import Text.Hakyll.CreateContext (createPage)

main = hakyll "http://example.com" $ do
    directory css "css"
    render "about.rst"
    render "index.markdown"
    render "code.lhs"
  where
    render = renderChain ["templates/default.html"]
           . createPage
