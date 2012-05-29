-- | Once a target is compiled, the user usually wants to save it to the disk.
-- This is where the 'Routes' type comes in; it determines where a certain
-- target should be written.
--
-- Suppose we have an item @foo\/bar.markdown@. We can render this to
-- @foo\/bar.html@ using:
--
-- > route "foo/bar.markdown" (setExtension ".html")
--
-- If we do not want to change the extension, we can use 'idRoute', the simplest
-- route available:
--
-- > route "foo/bar.markdown" idRoute
--
-- That will route @foo\/bar.markdown@ to @foo\/bar.markdown@.
--
-- Note that the extension says nothing about the content! If you set the
-- extension to @.html@, it is your own responsibility to ensure that the
-- content is indeed HTML.
--
-- Finally, some special cases:
--
-- * If there is no route for an item, this item will not be routed, so it will
--   not appear in your site directory.
--
-- * If an item matches multiple routes, the first rule will be chosen.
--
{-# LANGUAGE Rank2Types #-}
module Hakyll.Core.Routes
    ( Routes
    , runRoutes
    , idRoute
    , setExtension
    , matchRoute
    , customRoute
    , constRoute
    , gsubRoute
    , composeRoutes
    ) where

import Data.Monoid (Monoid, mempty, mappend)
import Control.Monad (mplus)
import System.FilePath (replaceExtension)

import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Util.String

-- | Type used for a route
--
newtype Routes = Routes {unRoutes :: forall a. Identifier a -> Maybe FilePath}

instance Monoid Routes where
    mempty = Routes $ const Nothing
    mappend (Routes f) (Routes g) = Routes $ \id' -> f id' `mplus` g id'

-- | Apply a route to an identifier
--
runRoutes :: Routes -> Identifier a -> Maybe FilePath
runRoutes = unRoutes

-- | A route that uses the identifier as filepath. For example, the target with
-- ID @foo\/bar@ will be written to the file @foo\/bar@.
--
idRoute :: Routes
idRoute = Routes $ Just . toFilePath

-- | Set (or replace) the extension of a route.
--
-- Example:
--
-- > runRoute (setExtension "html") "foo/bar"
--
-- Result:
--
-- > Just "foo/bar.html"
--
-- Example:
--
-- > runRoute (setExtension "html") "posts/the-art-of-trolling.markdown"
--
-- Result:
--
-- > Just "posts/the-art-of-trolling.html"
--
setExtension :: String -> Routes
setExtension extension = Routes $ fmap (`replaceExtension` extension)
                                . unRoutes idRoute

-- | Apply the route if the identifier matches the given pattern, fail
-- otherwise
--
matchRoute :: Pattern a -> Routes -> Routes
matchRoute pattern (Routes route) = Routes $ \id' ->
    if matches pattern (castIdentifier id') then route id' else Nothing

-- | Create a custom route. This should almost always be used with
-- 'matchRoute'
--
customRoute :: (Identifier a -> FilePath) -> Routes
customRoute f = Routes $ Just . f . castIdentifier

-- | A route that always gives the same result. Obviously, you should only use
-- this for a single compilation rule.
constRoute :: FilePath -> Routes
constRoute = customRoute . const

-- | Create a gsub route
--
-- Example:
--
-- > runRoutes (gsubRoute "rss/" (const "")) "tags/rss/bar.xml"
--
-- Result:
--
-- > Just "tags/bar.xml"
--
gsubRoute :: String              -- ^ Pattern
          -> (String -> String)  -- ^ Replacement
          -> Routes              -- ^ Resulting route
gsubRoute pattern replacement = customRoute $
    replaceAll pattern replacement . toFilePath

-- | Compose routes so that @f `composeRoutes` g@ is more or less equivalent
-- with @f >>> g@.
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
--
composeRoutes :: Routes  -- ^ First route to apply
              -> Routes  -- ^ Second route to apply
              -> Routes  -- ^ Resulting route
composeRoutes (Routes f) (Routes g) = Routes $ \i -> do
    p <- f i
    g $ parseIdentifier p
