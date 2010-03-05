module Text.Hakyll.Renderables
    ( createCustomPage
    , createListing
    , createListingWith
    , createPagePath
    , combine
    , combineWithUrl
    ) where

import qualified Data.Map as M
import Control.Arrow (second)
import Control.Monad (liftM2, mplus)
import Control.Applicative ((<$>))


import Text.Hakyll.File
import Text.Hakyll.Context
import Text.Hakyll.RenderAction
import Text.Hakyll.Render
import Text.Hakyll.Internal.Page

-- | Create a custom page.
--   
--   The association list given maps keys to values for substitution. Note
--   that as value, you can either give a @String@ or a @Hakyll String@.
--   A @Hakyll String@ is preferred for more complex data, since it allows
--   dependency checking. A @String@ is obviously more simple to use in some
--   cases.
createCustomPage :: String
                 -> [FilePath]
                 -> [(String, Either String (RenderAction () String))]
                 -> RenderAction () Context
createCustomPage url dependencies association = RenderAction
    { actionDependencies = dependencies
    , actionUrl          = Just $ return url
    , actionFunction     = \_ -> M.fromList <$> assoc'
    }
  where
    mtuple (a, b) = b >>= \b' -> return (a, b')
    toHakyllString = second (either return runRenderAction)
    assoc' = mapM (mtuple . toHakyllString) $ ("url", Left url) : association

-- | A @createCustomPage@ function specialized in creating listings.
--
--   This function creates a listing of a certain list of @Renderable@s. Every
--   item in the list is created by applying the given template to every
--   renderable. You can also specify additional context to be included in the
--   @CustomPage@.
--
--   > let customPage = createListingWith 
--   >                      "index.html" -- Destination of the page.
--   >                      "templates/postitem.html" -- Path to template to
--   >                                                -- render the items with.
--   >                      posts -- ^ Renderables to create the list with.
--   >                      [("title", "Home")] -- ^ Additional context
createListing :: String -- ^ Destination of the page.
              -> FilePath -- ^ Template to render all items with.
              -> [RenderAction () Context] -- ^ Renderables in the list.
              -> [(String, String)] -- ^ Additional context.
              -> RenderAction () Context
createListing = createListingWith id

-- | A @createCustomPage@ function specialized in creating listings.
--
--   In addition to @createListing@, this function allows you to specify an
--   extra @ContextManipulation@ for all @Renderable@s given.
createListingWith :: ContextManipulation -- ^ Manipulation for the renderables.
                  -> String -- ^ Destination of the page.
                  -> FilePath -- ^ Template to render all items with.
                  -> [RenderAction () Context] -- ^ Renderables in the list.
                  -> [(String, String)] -- ^ Additional context.
                  -> RenderAction () Context
createListingWith manipulation url template renderables additional =
    createCustomPage url dependencies context
  where
    dependencies = template : concatMap actionDependencies renderables
    context = ("body", Right concatenation) : additional'
    concatenation = renderAndConcatWith manipulation [template] renderables
    additional' = map (second Left) additional

-- | Create a PagePath from a FilePath.
createPagePath :: FilePath -> RenderAction () Context
createPagePath path = RenderAction
    { actionDependencies = [path]
    , actionUrl          = Just $ toUrl path
    , actionFunction     = const (readPage path)
    }

-- | Combine two renderables. The url will always be taken from the first
--   @Renderable@. Also, if a `$key` is present in both renderables, the
--   value from the first @Renderable@ will be taken as well.
--
--   Since renderables are always more or less key-value maps, you can see
--   this as a @union@ between two maps.
combine :: RenderAction () Context -> RenderAction () Context
        -> RenderAction () Context
combine x y = RenderAction
    { actionDependencies = actionDependencies x ++ actionDependencies y
    , actionUrl          = actionUrl x `mplus` actionUrl y
    , actionFunction     = \_ ->
        liftM2 M.union (runRenderAction x) (runRenderAction y)
    }

-- | Combine two renderables and set a custom URL. This behaves like @combine@,
--   except that for the @url@ field, the given URL is always chosen.
combineWithUrl :: FilePath
               -> RenderAction () Context
               -> RenderAction () Context
               -> RenderAction () Context
combineWithUrl url x y = combine'
    { actionUrl          = Just $ return url
    , actionFunction     = \_ ->
        M.insert "url" url <$> runRenderAction combine'
    }
  where
    combine' = combine x y
