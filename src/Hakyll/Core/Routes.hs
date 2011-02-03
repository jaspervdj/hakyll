-- | Once a target is compiled, the user usually wants to save it to the disk.
-- This is where the 'Routes' type comes in; it determines where a certain
-- target should be written.
--
-- When a route is applied (using 'runRoute'), it either returns a 'Just'
-- 'FilePath' (meaning the target should be written to that file path), or
-- 'Nothing' (meaning this target should not be written anywhere).
--
module Hakyll.Core.Routes
    ( Routes
    , runRoutes
    , idRoute
    , setExtension
    , ifMatch
    ) where

import Data.Monoid (Monoid, mempty, mappend)
import Control.Monad (mplus)
import System.FilePath (replaceExtension)

import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern

-- | Type used for a route
--
newtype Routes = Routes {unRoutes :: Identifier -> Maybe FilePath}

instance Monoid Routes where
    mempty = Routes $ const Nothing
    mappend (Routes f) (Routes g) = Routes $ \id' -> f id' `mplus` g id'

-- | Apply a route to an identifier
--
runRoutes :: Routes -> Identifier -> Maybe FilePath
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

-- | Modify a route: apply the route if the identifier matches the given
-- pattern, fail otherwise.
--
ifMatch :: Pattern -> Routes -> Routes
ifMatch pattern (Routes route) = Routes $ \id' ->
    if doesMatch pattern id' then route id'
                             else Nothing
