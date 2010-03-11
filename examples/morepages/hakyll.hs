import Text.Hakyll (hakyll)
import Text.Hakyll.File (directory)
import Text.Hakyll.Render (css, static, renderChain)
import Text.Hakyll.CreateContext (createPage, combine)

main = hakyll "http://example.com" $ do
    directory css "css"
    render "about.markdown"
    render "index.markdown"
    render "products.markdown"
  where
    render = renderChain ["templates/default.html"] . withFooter . createPage
    withFooter = flip combine $ createPage "footer.markdown"
