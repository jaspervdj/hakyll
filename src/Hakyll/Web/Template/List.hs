--------------------------------------------------------------------------------
-- | Provides an easy way to combine several items in a list. The applications
-- are obvious:
--
-- * A post list on a blog
--
-- * An image list in a gallery
--
-- * A sitemap
module Hakyll.Web.Template.List
    ( applyTemplateList
    , chronological
    , recentFirst
    ) where


--------------------------------------------------------------------------------
import           Data.List                   (sortBy)
import           Data.Ord                    (comparing)
import           System.FilePath             (takeBaseName)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Web.Template
import           Hakyll.Web.Template.Context


--------------------------------------------------------------------------------
-- | Set a field of a page to a listing of pages
applyTemplateList :: Template
                  -> Context a
                  -> [Item a]
                  -> Compiler String
applyTemplateList tpl context items = do
    items' <- mapM (applyTemplate tpl context) items
    return $ concat $ map itemBody items'


--------------------------------------------------------------------------------
-- | Sort pages chronologically. This function assumes that the pages have a
-- @year-month-day-title.extension@ naming scheme -- as is the convention in
-- Hakyll.
chronological :: [Item a] -> [Item a]
chronological = sortBy $ comparing $ takeBaseName . toFilePath . itemIdentifier


--------------------------------------------------------------------------------
-- | The reverse of 'chronological'
recentFirst :: [Item a] -> [Item a]
recentFirst = reverse . chronological
