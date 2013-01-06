--------------------------------------------------------------------------------
-- | This module containing some specialized functions to deal with tags. It
-- assumes you follow some conventions.
--
-- We support two types of tags: tags and categories.
--
-- To use default tags, use 'buildTags'. Tags are placed in a comma-separated
-- metadata field like this:
--
-- > ---
-- > author: Philip K. Dick
-- > title: Do androids dream of electric sheep?
-- > tags: future, science fiction, humanoid
-- > ---
-- > The novel is set in a post-apocalyptic near future, where the Earth and
-- > its populations have been damaged greatly by Nuclear...
--
-- To use categories, use the 'buildCategories' function. Categories are
-- determined by the direcetory a page is in, for example, the post
--
-- > posts/coding/2010-01-28-hakyll-categories.markdown
--
-- will receive the @coding@ category.
--
-- Advanced users may implement custom systems using 'buildTagsWith' if desired.
--
-- In the above example, we would want to create a page which lists all pages in
-- the @coding@ category, for example, with the 'Identifier':
--
-- > tags/coding.html
--
-- This is where the first parameter of 'buildTags' and 'buildCategories' comes
-- in. In the above case, we used the function:
--
-- > fromCapture "tags/*.html" :: String -> Identifier
--
-- The 'tagsRules' function lets you generate such a page for each tag in the
-- 'Rules' monad.
{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Hakyll.Web.Tags
    ( Tags
    , getTags
    , buildTagsWith
    , buildTags
    , buildCategories
    , tagsRules
    , renderTagCloud
    , renderTagList
    , tagsField
    , categoryField
    , sortTagsBy
    , caseInsensitiveTags
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   ((&&&))
import           Control.Monad                   (foldM, forM, forM_)
import           Data.Char                       (toLower)
import           Data.List                       (intercalate, intersperse,
                                                  sortBy)
import qualified Data.Map                        as M
import           Data.Maybe                      (catMaybes, fromMaybe)
import           Data.Monoid                     (mconcat)
import           Data.Ord                        (comparing)
import           System.FilePath                 (takeBaseName, takeDirectory)
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
import           Hakyll.Core.Rules
import           Hakyll.Core.Util.String
import           Hakyll.Web.Template.Context
import           Hakyll.Web.Html


--------------------------------------------------------------------------------
-- | Data about tags
data Tags = Tags
    { tagsMap        :: [(String, [Identifier])]
    , tagsMakeId     :: String -> Identifier
    , tagsDependency :: Dependency
    } deriving (Show)


--------------------------------------------------------------------------------
-- | Obtain tags from a page in the default way: parse them from the @tags@
-- metadata field.
getTags :: MonadMetadata m => Identifier -> m [String]
getTags identifier = do
    metadata <- getMetadata identifier
    return $ maybe [] (map trim . splitAll ",") $ M.lookup "tags" metadata

--------------------------------------------------------------------------------
-- | Obtain categories from a page.
getCategory :: MonadMetadata m => Identifier -> m [String]
getCategory = return . return . takeBaseName . takeDirectory . toFilePath


--------------------------------------------------------------------------------
-- | Higher-order function to read tags
buildTagsWith :: MonadMetadata m
              => (Identifier -> m [String])
              -> Pattern
              -> (String -> Identifier)
              -> m Tags
buildTagsWith f pattern makeId = do
    ids    <- getMatches pattern
    tagMap <- foldM addTags M.empty ids
    return $ Tags (M.toList tagMap) makeId (PatternDependency pattern ids)
  where
    -- Create a tag map for one page
    addTags tagMap id' = do
        tags <- f id'
        let tagMap' = M.fromList $ zip tags $ repeat [id']
        return $ M.unionWith (++) tagMap tagMap'


--------------------------------------------------------------------------------
buildTags :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildTags = buildTagsWith getTags


--------------------------------------------------------------------------------
buildCategories :: MonadMetadata m => Pattern -> (String -> Identifier)
                -> m Tags
buildCategories = buildTagsWith getCategory


--------------------------------------------------------------------------------
tagsRules :: Tags -> (String -> Pattern -> Rules ()) -> Rules ()
tagsRules tags rules =
    forM_ (tagsMap tags) $ \(tag, identifiers) ->
        create [tagsMakeId tags tag] $
            rulesExtraDependencies [tagsDependency tags] $
                rules tag $ fromList identifiers


--------------------------------------------------------------------------------
-- | Render tags in HTML
renderTags :: (String -> String -> Int -> Int -> Int -> String)
           -- ^ Produce a tag item: tag, url, count, min count, max count
           -> ([String] -> String)
           -- ^ Join items
           -> Tags
           -- ^ Tag cloud renderer
           -> Compiler String
renderTags makeHtml concatHtml tags = do
    -- In tags' we create a list: [((tag, route), count)]
    tags' <- forM (tagsMap tags) $ \(tag, ids) -> do
        route' <- getRoute $ tagsMakeId tags tag
        return ((tag, route'), length ids)

    -- TODO: We actually need to tell a dependency here!

    let -- Absolute frequencies of the pages
        freqs = map snd tags'

        -- The minimum and maximum count found
        (min', max')
            | null freqs = (0, 1)
            | otherwise  = (minimum &&& maximum) freqs

        -- Create a link for one item
        makeHtml' ((tag, url), count) =
            makeHtml tag (toUrl $ fromMaybe "/" url) count min' max'

    -- Render and return the HTML
    return $ concatHtml $ map makeHtml' tags'


--------------------------------------------------------------------------------
-- | Render a tag cloud in HTML
-- TODO: Maybe produce a Context here
renderTagCloud :: Double
               -- ^ Smallest font size, in percent
               -> Double
               -- ^ Biggest font size, in percent
               -> Tags
               -- ^ Input tags
               -> Compiler String
               -- ^ Rendered cloud
renderTagCloud minSize maxSize = renderTags makeLink (intercalate " ")
  where
    makeLink tag url count min' max' = renderHtml $
        H.a ! A.style (toValue $ "font-size: " ++ size count min' max')
            ! A.href (toValue url)
            $ toHtml tag

    -- Show the relative size of one 'count' in percent
    size count min' max' =
        let diff = 1 + fromIntegral max' - fromIntegral min'
            relative = (fromIntegral count - fromIntegral min') / diff
            size' = floor $ minSize + relative * (maxSize - minSize)
        in show (size' :: Int) ++ "%"


--------------------------------------------------------------------------------
-- | Render a simple tag list in HTML, with the tag count next to the item
-- TODO: Maybe produce a Context here
renderTagList :: Tags -> Compiler (String)
renderTagList = renderTags makeLink (intercalate ", ")
  where
    makeLink tag url count _ _ = renderHtml $
        H.a ! A.href (toValue url) $ toHtml (tag ++ " (" ++ show count ++ ")")


--------------------------------------------------------------------------------
-- | Render tags with links with custom function to get tags. It is typically
-- together with 'getTags' like this:
--
-- > renderTagsFieldWith (customFunction . getTags)
-- >     "tags" (fromCapture "tags/*")
tagsFieldWith :: (Identifier -> Compiler [String])  -- ^ Get the tags
              -> String                             -- ^ Destination key
              -> Tags                               -- ^ Tags structure
              -> Context a                          -- ^ Resulting context
tagsFieldWith getTags' key tags = field key $ \item -> do
    tags' <- getTags' $ itemIdentifier item
    links <- forM tags' $ \tag -> do
        route' <- getRoute $ tagsMakeId tags tag
        return $ renderLink tag route'

    return $ renderHtml $ mconcat $ intersperse ", " $ catMaybes $ links
  where
    -- Render one tag link
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) = Just $
        H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag


--------------------------------------------------------------------------------
-- | Render tags with links
tagsField :: String     -- ^ Destination key
          -> Tags       -- ^ Tags
          -> Context a  -- ^ Context
tagsField = tagsFieldWith getTags


--------------------------------------------------------------------------------
-- | Render the category in a link
categoryField :: String     -- ^ Destination key
              -> Tags       -- ^ Tags
              -> Context a  -- ^ Context
categoryField = tagsFieldWith getCategory


--------------------------------------------------------------------------------
-- | Sort tags using supplied function. First element of the tuple passed to
-- the comparing function is the actual tag name.
sortTagsBy :: ((String, [Identifier]) -> (String, [Identifier]) -> Ordering)
           -> Tags -> Tags
sortTagsBy f t = t {tagsMap = sortBy f (tagsMap t)}


--------------------------------------------------------------------------------
-- | Sample sorting function that compares tags case insensitively.
caseInsensitiveTags :: (String, [Identifier]) -> (String, [Identifier])
                    -> Ordering
caseInsensitiveTags = comparing $ map toLower . fst
