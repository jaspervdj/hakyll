--------------------------------------------------------------------------------
{- | 'Routes' is part of the 'Hakyll.Core.Rules.Rules' processing pipeline.
It determines if and where a compiled 'Hakyll.Core.Item.Item' is written out to
(relative to the output folder as configured in 'Hakyll.Core.Configuration.destinationDirectory').

* __If there is no route for an item, the compiled item won't be written out to a file__
and so won't appear in the output site directory.

* If an item matches multiple routes, the first route will be chosen.

__Examples__

Suppose we have a markdown item @foo\/bar.md@. We can route/output this to
@foo\/bar.html@ using:

> route "foo/bar.md" (setExtension ".html")

If we do not want to change the extension, we can use 'idRoute', the simplest
route available:

> route "foo/bar.md" idRoute

That will route @foo\/bar.md@ to @foo\/bar.md@.

Note: __The (output) extension says nothing about the content!__
If you set the extension to @.html@, you have to ensure that the compilation result
is indeed HTML (for example with the 'Hakyll.Web.Pandoc.pandocCompiler' to transform markdown to HTML).

Take a look at the built-in routes here for detailed usage examples.
-}
{-# LANGUAGE CPP        #-}
{-# LANGUAGE Rank2Types #-}
module Hakyll.Core.Routes
    ( Routes
    , UsedMetadata
    , runRoutes
    , idRoute
    , setExtension
    , matchRoute
    , customRoute
    , constRoute
    , gsubRoute
    , metadataRoute
    , composeRoutes
    ) where


--------------------------------------------------------------------------------
#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup                 (Semigroup (..))
#endif
import           System.FilePath                (replaceExtension, normalise)


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider
import           Hakyll.Core.Util.String


--------------------------------------------------------------------------------
-- | When you ran a route, it's useful to know whether or not this used
-- metadata. This allows us to do more granular dependency analysis.
type UsedMetadata = Bool


--------------------------------------------------------------------------------
data RoutesRead = RoutesRead
    { routesProvider   :: Provider
    , routesUnderlying :: Identifier
    }


--------------------------------------------------------------------------------
-- | Type used for a route
newtype Routes = Routes
    { unRoutes :: RoutesRead -> Identifier -> IO (Maybe FilePath, UsedMetadata)
    }


--------------------------------------------------------------------------------
#if MIN_VERSION_base(4,9,0)
instance Semigroup Routes where
    (<>) (Routes f) (Routes g) = Routes $ \p id' -> do
        (mfp, um) <- f p id'
        case mfp of
            Nothing -> g p id'
            Just _  -> return (mfp, um)

instance Monoid Routes where
    mempty  = Routes $ \_ _ -> return (Nothing, False)
    mappend = (<>)
#else
instance Monoid Routes where
    mempty = Routes $ \_ _ -> return (Nothing, False)
    mappend (Routes f) (Routes g) = Routes $ \p id' -> do
        (mfp, um) <- f p id'
        case mfp of
            Nothing -> g p id'
            Just _  -> return (mfp, um)
#endif


--------------------------------------------------------------------------------
-- | Apply a route to an identifier
runRoutes :: Routes -> Provider -> Identifier
          -> IO (Maybe FilePath, UsedMetadata)
runRoutes routes provider identifier =
    unRoutes routes (RoutesRead provider identifier) identifier


--------------------------------------------------------------------------------
{- | An "identity" route that interprets the identifier (of the item being processed) as the output filepath.
This identifier is normally the filepath of the
source file being processed. See 'Hakyll.Core.Identifier.Identifier' for details.

=== __Examples__
__Route when using match__

> -- e.g. file on disk: '<project-folder>/posts/hakyll.md'
> match "posts/*" $ do           -- 'hakyll.md' source file implicitly gets filepath as identifier: 'posts/hakyll.md'
>     route idRoute              -- so compilation result is written to '<output-folder>/posts/hakyll.md'
>     compile getResourceBody
-}
idRoute :: Routes
idRoute = customRoute toFilePath


--------------------------------------------------------------------------------
{- | Create a route like 'idRoute' that interprets the identifier (of the item being processed) as the output filepath
but also sets (or replaces) the extension suffix of that path.
This identifier is normally the filepath of the
source file being processed. See 'Hakyll.Core.Identifier.Identifier' for details.

=== __Examples__
__Route with an existing extension__

> -- e.g. file on disk: '<project-folder>/posts/hakyll.md'
> match "posts/*" $ do            -- 'hakyll.md' source file implicitly gets filepath as identifier: 'posts/hakyll.md'
>     route (setExtension "html") -- compilation result is written to '<output-folder>/posts/hakyll.html'
>     compile pandocCompiler

__Route without an existing extension__

> create ["about"] $ do           -- this implicitly gets identifier: 'about'
>     route (setExtension "html") -- compilation result is written to '<output-folder>/about.html'
>     compile $ makeItem ("Hello world" :: String)
-}
setExtension :: String -> Routes
setExtension extension = customRoute $
    (`replaceExtension` extension) . toFilePath


--------------------------------------------------------------------------------
-- | Apply the route if the identifier matches the given pattern, fail
-- otherwise
matchRoute :: Pattern -> Routes -> Routes
matchRoute pattern (Routes route) = Routes $ \p id' ->
    if matches pattern id' then route p id' else return (Nothing, False)


--------------------------------------------------------------------------------
{- | Create a route where you completely define the output filepath
(when the filepath only needs to depend on the identifier). 
This identifier is normally the filepath of the
source file being processed. See 'Hakyll.Core.Identifier.Identifier' for details.
This function should almost always be used with 'matchRoute'.

=== __Examples__
__Route that appends a custom extension__

> -- e.g. file on disk: '<project-folder>/posts/hakyll.md'
> match "posts/*" $ do            -- 'hakyll.md' source file implicitly gets filepath as identifier: 'posts/hakyll.md'
>     route $ customRoute ((<> ".html") . toFilePath) -- result is written to '<output-folder>/posts/hakyll.md.html'
>     compile pandocCompiler
Note that the last part of the output file path is @.md.html@
-}
customRoute :: (Identifier -> FilePath) -> Routes
customRoute f = Routes $ const $ \id' -> return (Just (f id'), False)


--------------------------------------------------------------------------------
{- | Create a route that writes the compiled item to the given output filepath
(ignoring any identifier or other data about the item being processed).
Obviously, you should use a specific output path only for a single file in a single compilation rule.

=== __Examples__
__Route to a specific filepath__

> -- e.g. file on disk: '<project-folder>/posts/hakyll.md'
> create ["main"] $ do                -- implicitly gets identifier: 'main' (ignored)
>     route $ constRoute "index.html" -- compilation result is written to '<output-folder>/index.html'
>     compile pandocCompiler
-}
constRoute :: FilePath -> Routes
constRoute = customRoute . const


--------------------------------------------------------------------------------
-- | Create a gsub route
--
-- Example:
--
-- > runRoutes (gsubRoute "rss/" (const "")) "tags/rss/bar.xml"
--
-- Result:
--
-- > Just "tags/bar.xml"
gsubRoute :: String              -- ^ Pattern
          -> (String -> String)  -- ^ Replacement
          -> Routes              -- ^ Resulting route
gsubRoute pattern replacement = customRoute $
    normalise . replaceAll pattern (replacement . removeWinPathSeparator) . removeWinPathSeparator . toFilePath
    where
        -- Filepaths on Windows containing `\\' will trip Regex matching, which
        -- is used in replaceAll. We normalise filepaths to have '/' as a path separator
        -- using removeWinPathSeparator


--------------------------------------------------------------------------------
-- | Get access to the metadata in order to determine the route
metadataRoute :: (Metadata -> Routes) -> Routes
metadataRoute f = Routes $ \r i -> do
    metadata <- resourceMetadata (routesProvider r) (routesUnderlying r)
    unRoutes (f metadata) r i


--------------------------------------------------------------------------------
-- | Compose routes so that @f \`composeRoutes\` g@ is more or less equivalent
-- with @g . f@.
--
-- Example:
--
-- > let routes = gsubRoute "rss/" (const "") `composeRoutes` setExtension "xml"
-- > in runRoutes routes "tags/rss/bar"
--
-- Result:
--
-- > Just "tags/bar.xml"
--
-- If the first route given fails, Hakyll will not apply the second route.
composeRoutes :: Routes  -- ^ First route to apply
              -> Routes  -- ^ Second route to apply
              -> Routes  -- ^ Resulting route
composeRoutes (Routes f) (Routes g) = Routes $ \p i -> do
    (mfp, um) <- f p i
    case mfp of
        Nothing -> return (Nothing, um)
        Just fp -> do
            (mfp', um') <- g p (fromFilePath fp)
            return (mfp', um || um')
