import Text.Hakyll (hakyll)
import Text.Hakyll.File (directory)
import Text.Hakyll.Render (css, static, renderChain)
import Text.Hakyll.Renderables (createPagePath)

main = hakyll $ do
    directory css "css"
    render "about.rst"
    render "index.markdown"
    render "code.lhs"
    where render = renderChain ["templates/default.html"] . createPagePath
