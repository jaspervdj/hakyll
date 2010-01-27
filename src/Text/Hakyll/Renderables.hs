module Text.Hakyll.Renderables
    ( CustomPage
    , createCustomPage
    , createListing
    , createListingWith
    , PagePath
    , createPagePath
    , CombinedRenderable
    , combine
    , combineWithURL
    ) where

import qualified Data.Map as M
import Control.Arrow (second)
import Control.Monad (liftM)

import Data.Binary

import Text.Hakyll.Hakyll (Hakyll)
import Text.Hakyll.Page
import Text.Hakyll.Renderable
import Text.Hakyll.File
import Text.Hakyll.Context
import Text.Hakyll.Render

-- | A custom page.
data CustomPage = CustomPage 
    { customPageURL :: String,
      customPageDependencies :: [FilePath],
      customPageContext :: [(String, Either String (Hakyll String))]
    }

-- | Create a custom page.
--   
--   The association list given maps keys to values for substitution. Note
--   that as value, you can either give a @String@ or a @Hakyll String@.
--   A @Hakyll String@ is preferred for more complex data, since it allows
--   dependency checking. A @String@ is obviously more simple to use in some
--   cases.
createCustomPage :: String -- ^ Destination of the page, relative to _site.
                 -> [FilePath] -- ^ Dependencies of the page.
                 -> [(String, Either String (Hakyll String))] -- ^ Mapping.
                 -> CustomPage
createCustomPage = CustomPage

-- | A @createCustomPage@ function specialized in creating listings.
--
--   This function creates a listing of a certain list of @Renderable@s. Every
--   item in the list is created by applying the given template to every
--   renderable. You can also specify additional context to be included in the
--   @CustomPage@.
createListing :: (Renderable a)
              => String -- ^ Destination of the page.
              -> FilePath -- ^ Template to render all items with.
              -> [a] -- ^ Renderables in the list.
              -> [(String, String)] -- ^ Additional context.
              -> CustomPage
createListing = createListingWith id

-- | A @createCustomPage@ function specialized in creating listings.
--
--   In addition to @createListing@, this function allows you to specify an
--   extra @ContextManipulation@ for all @Renderable@s given.
createListingWith :: (Renderable a)
                  => ContextManipulation -- ^ Manipulation for the renderables.
                  -> String -- ^ Destination of the page.
                  -> FilePath -- ^ Template to render all items with.
                  -> [a] -- ^ Renderables in the list.
                  -> [(String, String)] -- ^ Additional context.
                  -> CustomPage
createListingWith manipulation url template renderables additional =
    createCustomPage url dependencies context
  where
    dependencies = template : concatMap getDependencies renderables
    context = ("body", Right concatenation) : additional'
    concatenation = renderAndConcatWith manipulation [template] renderables
    additional' = map (second Left) additional

instance Renderable CustomPage where
    getDependencies = customPageDependencies
    getURL = customPageURL
    toContext page = do
        values <- mapM (either return id . snd) (customPageContext page)
        let pairs = zip (map fst $ customPageContext page) values
        return $ M.fromList $ ("url", customPageURL page) : pairs

-- | PagePath is a class that wraps a FilePath. This is used to render Pages
--   without reading them first through use of caching.
data PagePath = PagePath FilePath

-- | Create a PagePath from a FilePath.
createPagePath :: FilePath -> PagePath
createPagePath = PagePath

-- We can render filepaths
instance Renderable PagePath where
    getDependencies (PagePath path) = return path
    getURL (PagePath path) = toURL path
    toContext (PagePath path) = readPage path >>= toContext

-- We can serialize filepaths
instance Binary PagePath where
    put (PagePath path) = put path
    get = liftM PagePath get

-- | A combination of two other renderables.
data CombinedRenderable a b = CombinedRenderable a b
                            | CombinedRenderableWithURL FilePath a b

-- | Combine two renderables. The url will always be taken from the first
--   @Renderable@. Also, if a `$key` is present in both renderables, the
--   value from the first @Renderable@ will be taken as well.
--
--   Since renderables are always more or less key-value maps, you can see
--   this as a @union@ between two maps.
combine :: (Renderable a, Renderable b) => a -> b -> CombinedRenderable a b
combine = CombinedRenderable

-- | Combine two renderables and set a custom URL. This behaves like @combine@,
--   except that for the @url@ field, the given URL is always chosen.
combineWithURL :: (Renderable a, Renderable b)
               => FilePath
               -> a
               -> b
               -> CombinedRenderable a b
combineWithURL = CombinedRenderableWithURL

-- Render combinations.
instance (Renderable a, Renderable b)
         => Renderable (CombinedRenderable a b) where

    -- Add the dependencies.
    getDependencies (CombinedRenderable a b) =
        getDependencies a ++ getDependencies b
    getDependencies (CombinedRenderableWithURL _ a b) =
        getDependencies a ++ getDependencies b

    -- Take the url from the first renderable, or the specified URL.
    getURL (CombinedRenderable a _) = getURL a
    getURL (CombinedRenderableWithURL url _ _) = url

    -- Take a union of the contexts.
    toContext (CombinedRenderable a b) = do
        c1 <- toContext a
        c2 <- toContext b
        return $ c1 `M.union` c2
    toContext (CombinedRenderableWithURL url a b) = do
        c <- toContext (CombinedRenderable a b)
        return $ M.singleton "url" url `M.union` c
