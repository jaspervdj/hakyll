-- | Once a target is compiled, the user usually wants to save it to the disk.
-- This is where the 'Route' type comes in; it determines where a certain target
-- should be written.
--
-- When a route is applied (using 'runRoute'), it either returns a 'Just'
-- 'FilePath' (meaning the target should be written to that file path), or
-- 'Nothing' (meaning this target should not be written anywhere).
--
module Hakyll.Core.Route
    ( Route
    , runRoute
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
newtype Route = Route {unRoute :: Identifier -> Maybe FilePath}

instance Monoid Route where
    mempty = Route $ const Nothing
    mappend (Route f) (Route g) = Route $ \id' -> f id' `mplus` g id'

-- | Apply a route to an identifier
--
runRoute :: Route -> Identifier -> Maybe FilePath
runRoute = unRoute

-- | A route that uses the identifier as filepath. For example, the target with
-- ID @foo\/bar@ will be written to the file @foo\/bar@.
--
idRoute :: Route
idRoute = Route $ Just . toFilePath

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
setExtension :: String -> Route
setExtension extension = Route $ fmap (`replaceExtension` extension)
                               . unRoute idRoute

-- | Modify a route: apply the route if the identifier matches the given
-- pattern, fail otherwise.
--
ifMatch :: Pattern -> Route -> Route
ifMatch pattern (Route route) = Route $ \id' ->
    if doesMatch pattern id' then route id'
                             else Nothing
