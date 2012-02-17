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
    , resources
    , metaCompile
    , metaCompileWith
    , freshIdentifier
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Writer (tell)
import Control.Monad.Reader (ask, local)
import Control.Arrow ((>>>), arr, (>>^), (***))
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
             => [(Identifier a, Compiler () a)]
             -> Rules
tellCompilers compilers = RulesM $ do
    -- We box the compilers so they have a more simple type
    let compilers' = map (castIdentifier *** boxCompiler) compilers
    tell $ RuleSet mempty compilers' mempty
  where
    boxCompiler = (>>> arr compiledItem >>> arr CompileRule)

-- | Add resources
--
tellResources :: [Resource]
              -> Rules
tellResources resources' = RulesM $ tell $
    RuleSet mempty mempty $ S.fromList resources'

-- | Only compile/route items satisfying the given predicate
--
match :: Pattern a -> RulesM b -> RulesM b
match pattern = RulesM . local addPredicate . unRulesM
  where
    addPredicate env = env
        { rulesPattern = rulesPattern env `mappend` castPattern pattern
        }

-- | Greate a group of compilers
--
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
--
group :: String -> RulesM a -> RulesM a
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
        => Compiler Resource a -> RulesM (Pattern a)
compile compiler = do
    ids <- resources
    tellCompilers $ flip map ids $ \identifier ->
        (identifier, constA (fromIdentifier identifier) >>> compiler)
    tellResources $ map fromIdentifier ids
    return $ list ids
                   
-- | Add a compilation rule
--
-- This sets a compiler for the given identifier. No resource is needed, since
-- we are creating the item from scratch. This is useful if you want to create a
-- page on your site that just takes content from other items -- but has no
-- actual content itself. Note that the group of the given identifier is
-- replaced by the group set via 'group' (or 'Nothing', if 'group' has not been
-- used).
--
create :: (Binary a, Typeable a, Writable a)
       => Identifier a -> Compiler () a -> RulesM (Identifier a)
create id' compiler = RulesM $ do
    group' <- rulesGroup <$> ask
    let id'' = setGroup group' id'
    unRulesM $ tellCompilers [(id'', compiler)]
    return id''

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

-- | Get a list of resources matching the current pattern. This will also set
-- the correct group to the identifiers.
--
resources :: RulesM [Identifier a]
resources = RulesM $ do
    pattern <- rulesPattern <$> ask
    provider <- rulesResourceProvider <$> ask
    group' <- rulesGroup <$> ask
    return $ filterMatches pattern $ map (toId group') $ resourceList provider
  where
    toId g = setGroup g . toIdentifier

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
            => Compiler () [(Identifier a, Compiler () a)]   
            -- ^ Compiler generating the other compilers
            -> Rules
            -- ^ Resulting rules
metaCompile compiler = do
    id' <- freshIdentifier "Hakyll.Core.Rules.metaCompile"
    metaCompileWith id' compiler

-- | Version of 'metaCompile' that allows you to specify a custom identifier for
-- the metacompiler.
--
metaCompileWith :: (Binary a, Typeable a, Writable a)
                => Identifier ()
                -- ^ Identifier for this compiler
                -> Compiler () [(Identifier a, Compiler () a)]   
                -- ^ Compiler generating the other compilers
                -> Rules
                -- ^ Resulting rules
metaCompileWith identifier compiler = RulesM $ do
    group' <- rulesGroup <$> ask

    let -- Set the correct group on the identifier
        id' = setGroup group' identifier
        -- Function to box an item into a rule
        makeRule = MetaCompileRule . map (castIdentifier *** box)
        -- Entire boxing function
        box = (>>> fromDependency id' >>^ CompileRule . compiledItem)
        -- Resulting compiler list
        compilers = [(id', compiler >>> arr makeRule )]

    tell $ RuleSet mempty compilers mempty

-- | Generate a fresh Identifier with a given prefix
freshIdentifier :: String                 -- ^ Prefix
                -> RulesM (Identifier a)  -- ^ Fresh identifier
freshIdentifier prefix = RulesM $ do
    state <- get
    let index = rulesNextIdentifier state
        id'   = parseIdentifier $ prefix ++ "/" ++ show index
    put $ state {rulesNextIdentifier = index + 1}
    return id'
