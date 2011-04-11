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
--
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Hakyll.Core.Rules
    ( RulesM
    , Rules
    , match
    , group
    , compile
    , create
    , route
    , metaCompile
    , metaCompileWith
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Writer (tell)
import Control.Monad.Reader (ask, local)
import Control.Arrow (second, (>>>), arr, (>>^), (***))
import Control.Monad.State (get, put)
import Data.Monoid (mempty, mappend)
import qualified Data.Set as S

import Data.Typeable (Typeable)
import Data.Binary (Binary)

import Hakyll.Core.Resource
import Hakyll.Core.Resource.Provider
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Routes
import Hakyll.Core.CompiledItem
import Hakyll.Core.Writable
import Hakyll.Core.Rules.Internal
import Hakyll.Core.Util.Arrow

-- | Add a route
--
tellRoute :: Routes -> Rules
tellRoute route' = RulesM $ tell $ RuleSet route' mempty mempty

-- | Add a number of compilers
--
tellCompilers :: (Binary a, Typeable a, Writable a)
             => [(Identifier, Compiler () a)]
             -> Rules
tellCompilers compilers = RulesM $ do
    -- We box the compilers so they have a more simple type, and we apply the
    -- current group to the corresponding identifiers
    group' <- rulesGroup <$> ask
    let compilers' = map (setGroup group' *** boxCompiler) compilers
    tell $ RuleSet mempty compilers' mempty
  where
    boxCompiler = (>>> arr compiledItem >>> arr CompileRule)

-- | Add resources
--
tellResources :: [Resource]
              -> Rules
tellResources resources = RulesM $ tell $
    RuleSet mempty mempty $ S.fromList resources

-- | Only compile/route items satisfying the given predicate
--
match :: Pattern -> Rules -> Rules
match pattern = RulesM . local addPredicate . unRulesM
  where
    addPredicate env = env
        { rulesPattern = rulesPattern env `mappend` pattern
        }

-- | Greate a group of compilers
--
group :: String -> Rules -> Rules
group g = RulesM . local setGroup' . unRulesM
  where
    setGroup' env = env { rulesGroup = Just g }

-- | Add a compilation rule to the rules.
--
-- This instructs all resources to be compiled using the given compiler. When
-- no resources match the current selection, nothing will happen. In this case,
-- you might want to have a look at 'create'.
--
compile :: (Binary a, Typeable a, Writable a)
        => Compiler Resource a -> Rules
compile compiler = RulesM $ do
    pattern <- rulesPattern <$> ask
    provider <- rulesResourceProvider <$> ask
    let ids = filterMatches pattern $ map unResource $ resourceList provider
    unRulesM $ do
        tellCompilers $ flip map ids $ \identifier ->
            (identifier, constA (Resource identifier) >>> compiler)
        tellResources $ map Resource ids
                   
-- | Add a compilation rule
--
-- This sets a compiler for the given identifier. No resource is needed, since
-- we are creating the item from scratch. This is useful if you want to create a
-- page on your site that just takes content from other items -- but has no
-- actual content itself.
--
create :: (Binary a, Typeable a, Writable a)
       => Identifier -> Compiler () a -> Rules
create identifier compiler = tellCompilers [(identifier, compiler)]

-- | Add a route.
--
-- This adds a route for all items matching the current pattern.
--
route :: Routes -> Rules
route route' = RulesM $ do
    -- We want the route only to be applied if we match the current pattern and
    -- group
    pattern <- rulesPattern <$> ask
    group' <- rulesGroup <$> ask
    unRulesM $ tellRoute $ matchRoute (pattern `mappend` inGroup group') route'

-- | Apart from regular compilers, one is also able to specify metacompilers.
-- Metacompilers are a special class of compilers: they are compilers which
-- produce other compilers.
--
-- This is needed when the list of compilers depends on something we cannot know
-- before actually running other compilers. The most typical example is if we
-- have a blogpost using tags.
--
-- Every post has a collection of tags. For example,
--
-- > post1: code, haskell
-- > post2: code, random
--
-- Now, we want to create a list of posts for every tag. We cannot write this
-- down in our 'Rules' DSL directly, since we don't know what tags the different
-- posts will have -- we depend on information that will only be available when
-- we are actually compiling the pages.
--
-- The solution is simple, using 'metaCompile', we can add a compiler that will
-- parse the pages and produce the compilers needed for the different tag pages.
--
-- And indeed, we can see that the first argument to 'metaCompile' is a
-- 'Compiler' which produces a list of ('Identifier', 'Compiler') pairs. The
-- idea is simple: 'metaCompile' produces a list of compilers, and the
-- corresponding identifiers.
--
-- For simple hakyll systems, it is no need for this construction. More
-- formally, it is only needed when the content of one or more items determines
-- which items must be rendered.
--
metaCompile :: (Binary a, Typeable a, Writable a)
            => Compiler () [(Identifier, Compiler () a)]   
            -- ^ Compiler generating the other compilers
            -> Rules
            -- ^ Resulting rules
metaCompile compiler = RulesM $ do
    -- Create an identifier from the state
    state <- get
    let index = rulesMetaCompilerIndex state
        id' = fromCapture "Hakyll.Core.Rules.metaCompile/*" (show index)

    -- Update the state with a new identifier
    put $ state {rulesMetaCompilerIndex = index + 1}

    -- Fallback to 'metaCompileWith' with now known identifier
    unRulesM $ metaCompileWith id' compiler

-- | Version of 'metaCompile' that allows you to specify a custom identifier for
-- the metacompiler.
--
metaCompileWith :: (Binary a, Typeable a, Writable a)
                => Identifier
                -- ^ Identifier for this compiler
                -> Compiler () [(Identifier, Compiler () a)]   
                -- ^ Compiler generating the other compilers
                -> Rules
                -- ^ Resulting rules
metaCompileWith identifier compiler = RulesM $ do
    group' <- rulesGroup <$> ask

    let -- Set the correct group on the identifier
        id' = setGroup group' identifier
        -- Function to box an item into a rule
        makeRule = MetaCompileRule . map (second box)
        -- Entire boxing function
        box = (>>> fromDependency id' >>^ CompileRule . compiledItem)
        -- Resulting compiler list
        compilers = [(id', compiler >>> arr makeRule )]

    tell $ RuleSet mempty compilers mempty
