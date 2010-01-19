import Text.Hakyll (hakyll)
import Text.Hakyll.File (directory)
import Text.Hakyll.Render (css, static, renderChain)
import Text.Hakyll.Renderables (createPagePath, combine)

main = hakyll $ do
    directory css "css"
    render "about.markdown"
    render "index.markdown"
    render "products.markdown"
  where
    render = renderChain ["templates/default.html"] . withFooter . createPagePath
    withFooter a = a `combine` createPagePath "footer.markdown"
