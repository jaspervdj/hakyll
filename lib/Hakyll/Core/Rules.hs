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

The expanded, relative path of the matched source file on disk
(relative to the project directory configured with 'Hakyll.Core.Configuration.providerDirectory') becomes
the identifier under which the compilation result is saved to the 'Hakyll.Core.Store.Store'
(in case you want to 'Hakyll.Core.Compiler.load' it within another rule).
See 'Hakyll.Core.Identifier.Identifier' for details.

=== __Examples__
__Select all markdown files within a folder (including subfolders)__

> match "posts/**.md" $ do           -- Match all Markdown files in the 'posts' folder and subfolders
>     route $ setExtension "html"
>     compile pandocCompiler

__Match all markdown files within a folder (but without subfolders)__

> match "posts/*.md" $ do           -- Select all Markdown files in the 'posts' folder
>     route $ setExtension "html"
>     compile pandocCompiler
See 'Hakyll.Core.Identifier.Pattern.Pattern' or search online for "glob pattern" to get more details.
To control where the compilation result will be written out, use routing functions like 'Hakyll.Core.Routes.setExtension'.
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

The expanded, relative path of the matched source file on disk
(relative to the project directory configured with 'Hakyll.Core.Configuration.providerDirectory') becomes
the identifier under which the compilation result is saved to the 'Hakyll.Core.Store.Store'
(in case you want to 'Hakyll.Core.Compiler.load' it within another rule).
See 'Hakyll.Core.Identifier.Identifier' for details.

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
To control where the compilation result will be written out, use routing functions like 'Hakyll.Core.Routes.setExtension'.
-}
matchMetadata :: Pattern             -- ^ Glob pattern
              -> (Metadata -> Bool)  -- ^ Metadata predicate
              -> Rules ()            -- ^ Remaining processing pipeline
              -> Rules ()            -- ^ Resulting pipeline
matchMetadata pattern metadataPred = matchInternal pattern $
    map fst . filter (metadataPred . snd) <$> getAllMetadata pattern


--------------------------------------------------------------------------------
{- | Assign (and thereby create) the given identifier(s) to content that has no single,
underlying source file on disk. That content must be created within the given 'Rules' processing pipeline argument.
The given identifier is the id under which that content is saved to the 'Hakyll.Core.Store.Store'
(in case you want to 'Hakyll.Core.Compiler.load' it within another rule).
See 'Hakyll.Core.Identifier.Identifier' for details.

Use this function for example to create an overview page that doesn't have or need its content prepared in a file
(unlike blog posts which normally have a corresponding Markdown source file on disk).

=== __Examples__
__Create a webpage without an underlying source file__

> create ["index.html"] $ do                            -- saved with implicit identifier 'index.html' to Store
>     route idRoute                                     -- compilation result is written to '<output-folder>/index.html'
>     compile $ makeItem ("<h1>Hello World</h1>" :: String) -- create content that is also the "compilation result"
Note how you can use 'Hakyll.Core.Compiler.makeItem' to create content inline
(to be processed as a 'Hakyll.Core.Compiler.Compiler' value) as if that content was loaded from a file (as it's the
case when using 'match').
To control where the compilation result will be written out, use routing functions like 'Hakyll.Core.Routes.idRoute'.
-}
create :: [Identifier] -- ^ Identifiers to assign to created content in next argument
       -> Rules ()     -- ^ Remaining processing pipeline that must create content
       -> Rules ()     -- ^ Resulting pipeline
create ids rules = do
    flush
    -- TODO Maybe check if the resources exist and call tellResources on that
    Rules $ local setMatches $ unRules $ rules >> flush
  where
    setMatches env = env {rulesMatches = ids}


--------------------------------------------------------------------------------
{- | Add the given version name to the implicit identifier(s)
under which the compilation result of the given 'Rules' processing pipeline is saved to the 'Hakyll.Core.Store.Store'.
See 'Hakyll.Core.Identifier.Identifier' for details.

Use this wrapper function for example when you need to compile the same source file into two or more different outputs,
each with a different version name.
The version is needed to distinguish between these different outputs in the store,
otherwise they would get the same conflicting identifier in the store.

__If you add a version name with this function, you need to supply the same name__ when you
'Hakyll.Core.Compiler.load' the content from the store from within another rule.

=== __Examples__
__Compile source file into differently versioned outputs and load both__

> -- e.g. file on disk: 'posts/hakyll.md'
> match "posts/*" $ do                              -- saved with implicit identifier ('posts/hakyll.md', no-version)
>     route $ setExtension "html"
>     compile pandocCompiler
>
> match "posts/*" $ version "raw" $ do              -- saved with implicit identifier ('posts/hakyll.md', version 'raw')
>     route idRoute
>     compile getResourceBody
>
> create ["index.html"] $ do
>     route idRoute
>     compile $ do
>         compiledPost <- load (fromFilePath "posts/hakyll.md")                      -- load no-version version
>         rawPost <- load . setVersion (Just "raw") $ fromFilePath "posts/hakyll.md" -- load version 'raw'
>    ...
Note how a version name is needed to distinguish the normal and the "raw" version when loading the Hakyll post
for the @index.html@ page.
To control where the compilation result will be written out, use routing functions like 'Hakyll.Core.Routes.idRoute'.
and 'Hakyll.Core.Routes.setExtension'.
-}
version :: String   -- ^ Version name to add
        -> Rules () -- ^ Remaining processing pipeline
        -> Rules () -- ^ Resulting pipeline
version v rules = do
    flush
    Rules $ local setVersion' $ unRules $ rules >> flush
  where
    setVersion' env = env {rulesVersion = Just v}


--------------------------------------------------------------------------------
{- | Set or override transformation steps in a processing pipeline rule
with the given 'Hakyll.Core.Compiler.Compiler' value. So,
__this functions controls how the content (within a rule) is processed__
(use one of the 'match' functions to control what content is processed).

The compilation result is saved to the 'Hakyll.Core.Store.Store' under an implicit identifier.
See 'Hakyll.Core.Identifier.Identifier' for details.

If there's a route attached to the rule where this function is used, the compilation result is also written out
to a file according to that route.
See 'route' and 'Hakyll.Core.Routes.Routes' for details.

=== __Examples__
__Compile plain Markdown to HTML__

> match "posts/**.md" $ do         -- Select all Markdown files in 'posts' folder
>     route $ setExtension "html"
>     compile pandocCompiler       -- use pandoc to transform Markdown to HTML in a single step
Note how we set the content to be processed with 'Hakyll.Web.Pandoc.pandocCompiler'. The content 
comes implicitly from the matched Markdown files on disk. 
We don't have to pass that content around manually.
Every file is processed the same way within this one rule.

To control where the compilation result will be written out, use routing functions like 'Hakyll.Core.Routes.setExtension'.
Here the compilation result of a file like @posts\/hakyll.md@ is written out to @posts\/hakyll.html@.

__Compile Markdown to HTML within a template__

> match "posts/**.md" $ do           -- Select all Markdown files in 'posts' folder
>     route $ setExtension "html"
>     compile $
>         pandocCompiler >>= loadAndApplyTemplate "templates/post.html" defaultContext
Note how a Markdown post that is compiled to HTML using 'Hakyll.Web.Pandoc.pandocCompiler' can be embedded
into a wider HTMl frame called 'Hakyll.Web.Template.Template' (e.g. to control the overall design and layout of the
page or part of it) in a second step by using 'Hakyll.Web.Template.loadAndApplyTemplate'.
A template may look as follows:

> <h1>$title$</h1>
> $body$
See "Hakyll.Web.Template" to see examples of the templating syntax.
-}
compile :: (Binary a, Typeable a, Writable a) => Compiler (Item a) -- ^ How to transform content
                                              -> Rules ()          -- ^ Resulting processing pipeline
compile compiler = Rules $ modify $ \s ->
    s {rulesCompiler = Just (fmap SomeItem compiler)}


--------------------------------------------------------------------------------
{- | Create a processing pipeline (rule) by supplying the transformation steps in
form of the given 'Hakyll.Core.Compiler.Compiler'. So,
__this functions controls how the content (of this rule) is processed__
(use one of the 'match' functions to control what content is processed).

The compilation result is saved to the 'Hakyll.Core.Store.Store' under an implicit identifier.
See 'Hakyll.Core.Identifier.Identifier' for details.

If there's a route attached to the rule where this function is used, the compilation result is also written out
to a file according to that route.
See 'route' and 'Hakyll.Core.Routes.Routes' for details.

=== __Examples__
__Compile plain Markdown to HTML__

> match "posts/**.md" $ do         -- Select all Markdown files in 'posts' folder
>     route $ setExtension "html"
>     compile pandocCompiler       -- use pandoc to transform Markdown to HTML in a single step
Note how the content to process with 'Hakyll.Web.Pandoc.pandocCompiler' comes from the Markdown files under the hood.
You don't have to pass that content around manually.
To control where the compilation result will be written out, use routing functions like 'Hakyll.Core.Routes.setExtension'.
Here the compilation result of a file like @posts\/hakyll.md@ is written out to @posts\/hakyll.html@.

__Compile Markdown to HTML within a template__

> match "posts/**.md" $ do           -- Select all Markdown files in 'posts' folder
>     route $ setExtension "html"
>     compile $
>         pandocCompiler >>= loadAndApplyTemplate "templates/post.html" defaultContext
Note how a Markdown post that is compiled to HTML using 'Hakyll.Web.Pandoc.pandocCompiler' can be embedded
into a wider HTMl frame called 'Hakyll.Web.Template.Template' (e.g. to control the overall design and layout of the
page or part of it) in a second step by using 'Hakyll.Web.Template.loadAndApplyTemplate'.
A template may look as follows:

> <h1>$title$</h1>
> $body$
See "Hakyll.Web.Template" to see examples of the templating syntax.
-}
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
