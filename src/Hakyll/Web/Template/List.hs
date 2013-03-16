--------------------------------------------------------------------------------
-- | Provides an easy way to combine several items in a list. The applications
-- are obvious:
--
-- * A post list on a blog
--
-- * An image list in a gallery
--
-- * A sitemap
{-# LANGUAGE TupleSections #-}
module Hakyll.Web.Template.List
    ( applyTemplateList
    , applyJoinTemplateList
    , chronological
    , recentFirst
    ) where


--------------------------------------------------------------------------------
import           Control.Monad               (liftM)
import           Data.List                   (intersperse, sortBy)
import           Data.Ord                    (comparing)
import           System.Locale               (defaultTimeLocale)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
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
-- | Sort pages chronologically. Uses the same method as 'dateField' for
-- extracting the date.
chronological :: MonadMetadata m => [Item a] -> m [Item a]
chronological =
    sortByM $ getItemUTC defaultTimeLocale . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

--------------------------------------------------------------------------------
-- | The reverse of 'chronological'
recentFirst :: (MonadMetadata m, Functor m) => [Item a] -> m [Item a]
recentFirst = fmap reverse . chronological
