--------------------------------------------------------------------------------
-- | This module provides a declarative DSL in which the user can specify the
-- different rules used to run the compilers.
--
-- The convention is to just list all items in the 'Rules' monad, routes and
-- compilation rules.
--
-- A typical usage example would be:
--
-- > main = hakyll $ do
-- >     match "posts/*" $ do
-- >         route   (setExtension "html")
-- >         compile someCompiler
-- >     match "css/*" $ do
-- >         route   idRoute
-- >         compile compressCssCompiler
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Hakyll.Core.Rules
    ( Rules
    , match
    , version
    , compile
    , route
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            ((<$>))
import           Control.Arrow                  (second)
import           Control.Monad.Reader           (ask, local)
import           Control.Monad.Writer           (tell)
import           Data.Monoid                    (mappend, mempty)
import qualified Data.Set                       as S


--------------------------------------------------------------------------------
import           Data.Binary                    (Binary)
import           Data.Typeable                  (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Item
import           Hakyll.Core.Item.SomeItem
import           Hakyll.Core.Metadata
import           Hakyll.Core.Routes
import           Hakyll.Core.Rules.Internal
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
-- | Add a route
tellRoute :: Routes -> Rules ()
tellRoute route' = Rules $ tell $ RuleSet route' mempty mempty


--------------------------------------------------------------------------------
-- | Add a number of compilers
tellCompilers :: (Binary a, Typeable a, Writable a)
             => [(Identifier, Compiler (Item a))]
             -> Rules ()
tellCompilers compilers = Rules $ do
    -- We box the compilers so they have a more simple type
    let compilers' = map (second $ fmap SomeItem) compilers
    tell $ RuleSet mempty compilers' mempty


--------------------------------------------------------------------------------
-- | Add resources
tellResources :: [Identifier]
              -> Rules ()
tellResources resources' = Rules $ tell $
    RuleSet mempty mempty $ S.fromList resources'


--------------------------------------------------------------------------------
match :: Pattern -> Rules b -> Rules b
match pattern = Rules . local addPattern . unRules
  where
    addPattern env = env
        { rulesPattern = rulesPattern env `mappend` pattern
        }


--------------------------------------------------------------------------------
version :: String -> Rules a -> Rules a
version v = Rules . local setVersion' . unRules
  where
    setVersion' env = env {rulesVersion = Just v}


--------------------------------------------------------------------------------
-- | Add a compilation rule to the rules.
--
-- This instructs all resources to be compiled using the given compiler. When
-- no resources match the current selection, nothing will happen. In this case,
-- you might want to have a look at 'create'.
compile :: (Binary a, Typeable a, Writable a)
        => Compiler (Item a) -> Rules ()
compile compiler = do
    pattern  <- Rules $ rulesPattern <$> ask
    version' <- Rules $ rulesVersion <$> ask
    ids      <- case fromLiteral pattern of
        Just id' -> return [id']
        Nothing  -> do
            ids <- getMatches pattern
            tellResources ids
            return ids

    tellCompilers [(setVersion version' id', compiler) | id' <- ids]


--------------------------------------------------------------------------------
-- | Add a route.
--
-- This adds a route for all items matching the current pattern.
route :: Routes -> Rules ()
route route' = Rules $ do
    -- We want the route only to be applied if we match the current pattern and
    -- version
    pattern  <- rulesPattern <$> ask
    version' <- rulesVersion <$> ask
    unRules $ tellRoute $ matchRoute
        (pattern `mappend` fromVersion version') route'
