--------------------------------------------------------------------------------
{- | This module provides a declarative Domain Specific Language (DSL) to generate a static site
by specifying transformation 'Rules' (although the use case is not limited to static sites).
__Each rule is a processing pipeline__ which normally consists of three parts:

1. Inputs (like Markdown files) to process (collected with e.g. 'match' or 'create').
2. Transformation steps (like Markdown to HTML) to compute ("compile") for each input its output content
(collected with 'compile' and 'Hakyll.Core.Compiler.Compiler').
3. Routing to determine to which file an output content will be written out. For a static site
this translates to determine under which URL the output content will be accessible
(configured with 'route' and 'Hakyll.Core.Routes.Routes').

A typical usage example looks as follows:

> {-# LANGUAGE OverloadedStrings #-}    -- write 'match "posts/**.md"' instead of 'match $ fromGlob "posts/**.md"'
> ...
>
> main = hakyll $ do
>     -- Rule 1
>     match "posts/**.md" $ do          -- Inputs: all Markdown files like 'hakyll.md' in the 'posts' folder
>         route $ setExtension "html"   -- Routing: Only replace extension, so '<output-folder>/posts/hakyll.html'.
>         compile pandocCompiler        -- Transformation step(s): Compile Markdown inputs to HTML outputs.
>     -- Rule 2
>     match "css/*" $ do
>         route idRoute
>         compile compressCssCompiler
>     ...
__The order of the rules doesn't matter.__
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Hakyll.Core.Rules
    ( Rules
    , match
    , matchMetadata
    , create
    , version
    , compile
    , route

      -- * Advanced usage
    , preprocess
    , Dependency (..)
    , rulesExtraDependencies
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Reader           (ask, local)
import           Control.Monad.State            (get, modify, put)
import           Control.Monad.Trans            (liftIO)
import           Control.Monad.Writer           (censor, tell)
import           Data.Maybe                     (fromMaybe)
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
tellRoute route' = Rules $ tell $ RuleSet route' mempty mempty mempty


--------------------------------------------------------------------------------
-- | Add a number of compilers
tellCompilers :: [(Identifier, Compiler SomeItem)] -> Rules ()
tellCompilers compilers = Rules $ tell $ RuleSet mempty compilers mempty mempty


--------------------------------------------------------------------------------
-- | Add resources
tellResources :: [Identifier] -> Rules ()
tellResources resources' = Rules $ tell $
    RuleSet mempty mempty (S.fromList resources') mempty


--------------------------------------------------------------------------------
-- | Add a pattern
tellPattern :: Pattern -> Rules ()
tellPattern pattern = Rules $ tell $ RuleSet mempty mempty mempty pattern


--------------------------------------------------------------------------------
flush :: Rules ()
flush = Rules $ do
    mcompiler <- rulesCompiler <$> get
    case mcompiler of
        Nothing       -> return ()
        Just compiler -> do
            matches' <- rulesMatches                  <$> ask
            version' <- rulesVersion                  <$> ask
            route'   <- fromMaybe mempty . rulesRoute <$> get

            -- The version is possibly not set correctly at this point (yet)
            let ids = map (setVersion version') matches'

            {-
            ids      <- case fromLiteral pattern of
                Just id' -> return [setVersion version' id']
                Nothing  -> do
                    ids <- unRules $ getMatches pattern
                    unRules $ tellResources ids
                    return $ map (setVersion version') ids
            -}

            -- Create a fast pattern for routing that matches exactly the
            -- compilers created in the block given to match
            let fastPattern = fromList ids

            -- Write out the compilers and routes
            unRules $ tellRoute $ matchRoute fastPattern route'
            unRules $ tellCompilers $ [(id', compiler) | id' <- ids]

    put $ emptyRulesState


--------------------------------------------------------------------------------
matchInternal :: Pattern -> Rules [Identifier] -> Rules () -> Rules ()
matchInternal pattern getIDs rules = do
    tellPattern pattern
    flush
    ids <- getIDs
    tellResources ids
    Rules $ local (setMatches ids) $ unRules $ rules >> flush
  where
    setMatches ids env = env {rulesMatches = ids}

--------------------------------------------------------------------------------
{- | Add a selection of which input files to process (using the given
[glob pattern](https://w.wiki/4Wsx)) to the given remaining 'Rules' pipeline.

=== __Examples__
__Select all markdown files within a folder (including subfolders)__

> match "posts/**.md" $ do           -- Match all Markdown files in the 'posts' folder and subfolders
>     route $ setExtension "html"
>     compile pandocCompiler

__Match all markdown files within a folder (but without subfolders)__

> match "posts/*.md" $ do           -- Select all Markdown files in the 'posts' folder
>     route $ setExtension "html"
>     compile pandocCompiler
Take a look at 'Hakyll.Core.Identifier.Pattern.Pattern' or search online for "glob pattern" to get more details.
-}
match :: Pattern  -- ^ Glob pattern
      -> Rules () -- ^ Remaining processing pipeline
      -> Rules () -- ^ Resulting pipeline
match pattern = matchInternal pattern $ getMatches pattern


--------------------------------------------------------------------------------
{- | Add a selection of which input files to process (using the given
[glob pattern](https://w.wiki/4Wsx) and metadata predicate) to the given remaining 'Rules' pipeline.
Same as 'match' but allows to filter files further based on their (metadata) content
(a file is added only when the metadata predicate returns @True@).

=== __Examples__
__Select all markdown files with enabled draft flag within a folder__

> matchMetadata "posts/*.md" (\meta -> maybe False (=="true") $ lookupString "draft" meta) $ do
>     route $ setExtension "html"
>     compile pandocCompiler

For example, the following 'posts/hakyll.md' file with @draft: true@ metadata would match:

> ---
> draft: true
> title: Hakyll Post
> ...
> ---
> In this blog post we learn about Hakyll ...
Note that files that have @draft: false@ or no such draft field at all, would not match.
You can use helper functions like 'Hakyll.Core.Metadata.lookupString' to access a specific metadata field, and 
'Data.Maybe.maybe' to work with 'Data.Maybe.Maybe'.
-}
matchMetadata :: Pattern             -- ^ Glob pattern
              -> (Metadata -> Bool)  -- ^ Metadata predicate
              -> Rules ()            -- ^ Remaining processing pipeline
              -> Rules ()            -- ^ Resulting pipeline
matchMetadata pattern metadataPred = matchInternal pattern $
    map fst . filter (metadataPred . snd) <$> getAllMetadata pattern


--------------------------------------------------------------------------------
create :: [Identifier] -> Rules () -> Rules ()
create ids rules = do
    flush
    -- TODO Maybe check if the resources exist and call tellResources on that
    Rules $ local setMatches $ unRules $ rules >> flush
  where
    setMatches env = env {rulesMatches = ids}


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
-- This instructs all resources to be compiled using the given compiler.
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
-- | Execute an 'IO' action immediately while the rules are being evaluated.
-- This should be avoided if possible, but occasionally comes in useful.
preprocess :: IO a -> Rules a
preprocess = Rules . liftIO


--------------------------------------------------------------------------------
-- | Advanced usage: add extra dependencies to compilers. Basically this is
-- needed when you're doing unsafe tricky stuff in the rules monad, but you
-- still want correct builds.
--
-- A useful utility for this purpose is 'makePatternDependency'.
rulesExtraDependencies :: [Dependency] -> Rules a -> Rules a
rulesExtraDependencies deps rules =
    -- Note that we add the dependencies seemingly twice here. However, this is
    -- done so that 'rulesExtraDependencies' works both if we have something
    -- like:
    --
    -- > match "*.css" $ rulesExtraDependencies [foo] $ ...
    --
    -- and something like:
    --
    -- > rulesExtraDependencies [foo] $ match "*.css" $ ...
    --
    -- (1) takes care of the latter and (2) of the former.
    Rules $ censor fixRuleSet $ do
        x <- unRules rules
        fixCompiler
        return x
  where
    -- (1) Adds the dependencies to the compilers we are yet to create
    fixCompiler = modify $ \s -> case rulesCompiler s of
        Nothing -> s
        Just c  -> s
            { rulesCompiler = Just $ compilerTellDependencies deps >> c
            }

    -- (2) Adds the dependencies to the compilers that are already in the ruleset
    fixRuleSet ruleSet = ruleSet
        { rulesCompilers =
            [ (i, compilerTellDependencies deps >> c)
            | (i, c) <- rulesCompilers ruleSet
            ]
        }
