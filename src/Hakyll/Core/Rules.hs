--------------------------------------------------------------------------------
-- | This module provides a declarative DSL in which the user can specify the
-- different rules used to run the compilers.
--
-- The convention is to just list all items in the 'RulesM' monad, routes and
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
    ( RulesM
    , Rules
    , match
    , group
    , compile
    , create
    , route
    , resources
    , freshIdentifier
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            ((<$>))
import           Control.Arrow                  (arr, (***), (>>>))
import           Control.Monad.Reader           (ask, local)
import           Control.Monad.State            (get, put)
import           Control.Monad.Writer           (tell)
import           Data.Monoid                    (mappend, mempty)
import qualified Data.Set                       as S


--------------------------------------------------------------------------------
import           Data.Binary                    (Binary)
import           Data.Typeable                  (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.CompiledItem
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.ResourceProvider
import           Hakyll.Core.Routes
import           Hakyll.Core.Rules.Internal
import           Hakyll.Core.Writable


--------------------------------------------------------------------------------
-- | Add a route
tellRoute :: Routes -> Rules
tellRoute route' = RulesM $ tell $ RuleSet route' mempty mempty


--------------------------------------------------------------------------------
-- | Add a number of compilers
tellCompilers :: (Binary a, Typeable a, Writable a)
             => [(Identifier a, Compiler () a)]
             -> Rules
tellCompilers compilers = RulesM $ do
    -- We box the compilers so they have a more simple type
    let compilers' = map (castIdentifier *** boxCompiler) compilers
    tell $ RuleSet mempty compilers' mempty
  where
    boxCompiler = (>>> arr compiledItem)


--------------------------------------------------------------------------------
-- | Add resources
tellResources :: [Identifier a]
              -> Rules
tellResources resources' = RulesM $ tell $
    RuleSet mempty mempty $ S.fromList $ map castIdentifier resources'


--------------------------------------------------------------------------------
-- | Only compile/route items satisfying the given predicate
match :: Pattern a -> RulesM b -> RulesM b
match pattern = RulesM . local addPredicate . unRulesM
  where
    addPredicate env = env
        { rulesPattern = rulesPattern env `mappend` castPattern pattern
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
group :: String -> RulesM a -> RulesM a
group g = RulesM . local setGroup' . unRulesM
  where
    setGroup' env = env { rulesGroup = Just g }


--------------------------------------------------------------------------------
-- | Add a compilation rule to the rules.
--
-- This instructs all resources to be compiled using the given compiler. When
-- no resources match the current selection, nothing will happen. In this case,
-- you might want to have a look at 'create'.
compile :: (Binary a, Typeable a, Writable a)
        => Compiler () a -> RulesM (Pattern a)
compile compiler = do
    ids <- resources
    tellCompilers [(castIdentifier id', compiler) | id' <- ids]
    tellResources ids
    return $ list $ map castIdentifier ids


--------------------------------------------------------------------------------
-- | Add a compilation rule
--
-- This sets a compiler for the given identifier. No resource is needed, since
-- we are creating the item from scratch. This is useful if you want to create a
-- page on your site that just takes content from other items -- but has no
-- actual content itself. Note that the group of the given identifier is
-- replaced by the group set via 'group' (or 'Nothing', if 'group' has not been
-- used).
create :: (Binary a, Typeable a, Writable a)
       => Identifier a -> Compiler () a -> RulesM (Identifier a)
create id' compiler = RulesM $ do
    group' <- rulesGroup <$> ask
    let id'' = setGroup group' id'
    unRulesM $ tellCompilers [(id'', compiler)]
    return id''


--------------------------------------------------------------------------------
-- | Add a route.
--
-- This adds a route for all items matching the current pattern.
route :: Routes -> Rules
route route' = RulesM $ do
    -- We want the route only to be applied if we match the current pattern and
    -- group
    pattern <- rulesPattern <$> ask
    group' <- rulesGroup <$> ask
    unRulesM $ tellRoute $ matchRoute (pattern `mappend` inGroup group') route'


--------------------------------------------------------------------------------
-- | Get a list of resources matching the current pattern. This will also set
-- the correct group to the identifiers.
resources :: RulesM [Identifier ()]
resources = RulesM $ do
    pattern  <- rulesPattern <$> ask
    provider <- rulesResourceProvider <$> ask
    g        <- rulesGroup <$> ask
    return $ filterMatches pattern $ map (setGroup g) $ resourceList provider


--------------------------------------------------------------------------------
-- | Generate a fresh Identifier with a given prefix
-- TODO: remove?
freshIdentifier :: String                 -- ^ Prefix
                -> RulesM (Identifier a)  -- ^ Fresh identifier
freshIdentifier prefix = RulesM $ do
    state <- get
    let index = rulesNextIdentifier state
        id'   = parseIdentifier $ prefix ++ "/" ++ show index
    put $ state {rulesNextIdentifier = index + 1}
    return id'
