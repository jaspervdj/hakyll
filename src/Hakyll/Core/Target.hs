-- | A target represents one compilation unit, e.g. a blog post, a CSS file...
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Target
    ( DependencyLookup
    , TargetM
    , Target
    , runTarget
    ) where

import Hakyll.Core.Target.Internal
