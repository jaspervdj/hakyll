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

      -- * Advanced usage
    , rulesExtraDependencies
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            ((<$>))
import           Control.Monad.Reader           (ask, local)
import           Control.Monad.State            (get, modify, put)
import           Control.Monad.Writer           (censor, tell)
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    (mappend, mempty)
import qualified Data.Set                       as S


--------------------------------------------------------------------------------
import           Data.Binary                    (Binary)
import           Data.Typeable                  (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Dependencies
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
tellCompilers :: [(Identifier, Compiler SomeItem)] -> Rules ()
tellCompilers compilers = Rules $ tell $ RuleSet mempty compilers mempty


--------------------------------------------------------------------------------
-- | Add resources
tellResources :: [Identifier] -> Rules ()
tellResources resources' = Rules $ tell $
    RuleSet mempty mempty $ S.fromList resources'


--------------------------------------------------------------------------------
flush :: Rules ()
flush = Rules $ do
    mcompiler <- rulesCompiler <$> get
    case mcompiler of
        Nothing       -> return ()
        Just compiler -> do
            pattern  <- rulesPattern                  <$> ask
            version' <- rulesVersion                  <$> ask
            route'   <- fromMaybe mempty . rulesRoute <$> get
            ids      <- case fromLiteral pattern of
                Just id' -> return [setVersion version' id']
                Nothing  -> do
                    ids <- unRules $ getMatches pattern
                    unRules $ tellResources ids
                    return $ map (setVersion version') ids

            -- Create a fast pattern for routing that matches exactly the
            -- compilers created in the block given to match
            let fastPattern = fromList ids

            -- Write out the compilers and routes
            unRules $ tellRoute $ matchRoute fastPattern route'
            unRules $ tellCompilers $ [(id', compiler) | id' <- ids]

    put $ emptyRulesState


--------------------------------------------------------------------------------
match :: Pattern -> Rules () -> Rules ()
match pattern rules = do
    flush
    Rules $ local addPattern $ unRules $ rules >> flush
  where
    addPattern env = env
        { rulesPattern = rulesPattern env `mappend` pattern
        }


--------------------------------------------------------------------------------
version :: String -> Rules () -> Rules ()
version v rules = do
    flush
    Rules $ local setVersion' $ unRules $ rules >> flush
  where
    setVersion' env = env {rulesVersion = Just v}


--------------------------------------------------------------------------------
-- | Add a compilation rule to the rules.
--
-- This instructs all resources to be compiled using the given compiler. When
-- no resources match the current selection, nothing will happen. In this case,
-- you might want to have a look at 'create'.
compile :: (Binary a, Typeable a, Writable a) => Compiler (Item a) -> Rules ()
compile compiler = Rules $ modify $ \s ->
    s {rulesCompiler = Just (fmap SomeItem compiler)}


--------------------------------------------------------------------------------
-- | Add a route.
--
-- This adds a route for all items matching the current pattern.
route :: Routes -> Rules ()
route route' = Rules $ modify $ \s -> s {rulesRoute = Just route'}


--------------------------------------------------------------------------------
-- | Advanced usage: add extra dependencies to compilers. Basically this is
-- needed when you're doing unsafe tricky stuff in the rules monad, but you
-- still want correct builds.
rulesExtraDependencies :: [Dependency] -> Rules a -> Rules a
rulesExtraDependencies deps = Rules . censor addDependencies . unRules
  where
    -- Adds the dependencies to the compilers in the ruleset
    addDependencies ruleSet = ruleSet
        { rulesCompilers =
            [ (i, compilerTellDependencies deps >> c)
            | (i, c) <- rulesCompilers ruleSet
            ]
        }
