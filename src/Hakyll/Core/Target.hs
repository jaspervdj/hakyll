-- | A target represents one compilation unit, e.g. a blog post, a CSS file...
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Target
    ( DependencyLookup
    , TargetM
    , runTarget
    , getIdentifier
    , getResourceString
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)

import Hakyll.Core.Identifier
import Hakyll.Core.Target.Internal
import Hakyll.Core.ResourceProvider

-- | Get the current identifier
--
getIdentifier :: TargetM Identifier
getIdentifier = TargetM $ targetIdentifier <$> ask

-- | Get the resource content as a string
--
getResourceString :: TargetM String
getResourceString = TargetM $ do
    provider <- targetResourceProvider <$> ask
    identifier <- unTargetM getIdentifier
    liftIO $ resourceString provider identifier
