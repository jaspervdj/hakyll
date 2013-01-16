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
    , applyJoinTemplateList
    , chronological
    , recentFirst
    ) where


--------------------------------------------------------------------------------
import           Data.List                   (intersperse, sortBy)
import           Data.Ord                    (comparing)
import           System.FilePath             (takeBaseName)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Web.Template
import           Hakyll.Web.Template.Context


--------------------------------------------------------------------------------
-- | Generate a string of a listing of pages, after applying a template to each
-- page.
applyTemplateList :: Template
                  -> Context a
                  -> [Item a]
                  -> Compiler String
applyTemplateList = applyJoinTemplateList ""


--------------------------------------------------------------------------------
-- | Join a listing of pages with a string in between, after applying a template
-- to each page.
applyJoinTemplateList :: String
                      -> Template
                      -> Context a
                      -> [Item a]
                      -> Compiler String
applyJoinTemplateList delimiter tpl context items = do
    items' <- mapM (applyTemplate tpl context) items
    return $ concat $ intersperse delimiter $ map itemBody items'


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
