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
    , group
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
import           Hakyll.Core.Provider
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
-- | Greate a group of compilers
-- Imagine you have a page that you want to render, but you also want the raw
-- content available on your site.
--
-- > match "test.markdown" $ do
-- >     route $ setExtension "html"
-- >     compile pageCompiler
-- >
-- > match "test.markdown" $ do
-- >     route idRoute
-- >     compile copyPageCompiler
--
-- Will of course conflict! In this case, Hakyll will pick the first matching
-- compiler (@pageCompiler@ in this case).
--
-- In case you want to have them both, you can use the 'group' function to
-- create a new group. For example,
--
-- > match "test.markdown" $ do
-- >     route $ setExtension "html"
-- >     compile pageCompiler
-- >
-- > group "raw" $ do
-- >     match "test.markdown" $ do
-- >         route idRoute
-- >         compile copyPageCompiler
--
-- This will put the compiler for the raw content in a separate group
-- (@\"raw\"@), which causes it to be compiled as well.
group :: String -> Rules a -> Rules a
group g = Rules . local setVersion' . unRules
  where
    setVersion' env = env {rulesVersion = Just g}


--------------------------------------------------------------------------------
-- | Add a compilation rule to the rules.
--
-- This instructs all resources to be compiled using the given compiler. When
-- no resources match the current selection, nothing will happen. In this case,
-- you might want to have a look at 'create'.
compile :: (Binary a, Typeable a, Writable a)
        => Compiler (Item a) -> Rules ()
compile compiler = do
    pattern <- Rules $ rulesPattern <$> ask
    ids     <- case fromLiteral pattern of
        Just id' -> return [id']
        Nothing  -> do
            ids <- resources
            tellResources ids
            return ids

    tellCompilers [(id', compiler) | id' <- ids]


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


--------------------------------------------------------------------------------
-- | Get a list of resources matching the current pattern. This will also set
-- the correct group to the identifiers.
-- TODO: Make this private?
resources :: Rules [Identifier]
resources = Rules $ do
    pattern  <- rulesPattern  <$> ask
    provider <- rulesProvider <$> ask
    g        <- rulesVersion  <$> ask
    return $ filterMatches pattern $ map (setVersion g) $ resourceList provider
