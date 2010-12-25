-- | A target represents one compilation unit, e.g. a blog post, a CSS file...
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Target
    ( DependencyLookup
    , TargetM
    , Target
    , runTarget
    , getResourceString
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)

import Hakyll.Core.Target.Internal
import Hakyll.Core.ResourceProvider

-- | Get the resource content as a string
--
getResourceString :: TargetM a String
getResourceString = TargetM $ do
    provider <- targetResourceProvider <$> ask
    identifier <- targetIdentifier <$> ask
    liftIO $ resourceString provider identifier
