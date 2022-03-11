--------------------------------------------------------------------------------
-- | This module provides a declarative Domain Specific Language (DSL) to
-- generate a static site by specifying transformation 'Rules' (although the
-- use case is not limited to static sites).
-- Each rule normally consists of three parts:
-- 
-- 1. Source files (like Markdown files) to process (collected with e.g.
-- 'match' or 'create').
-- 2. Compilation steps (like Markdown to HTML) to transform files' content
-- to some output content (steps are collected within 
-- 'Hakyll.Core.Compiler.Compiler' and executed with 'compile').
-- 3. Routing to determine if and where an output content will be written out. 
-- For a static site this determines under which URL the output content will 
-- be available (configured with 'route' and 'Hakyll.Core.Routes.Routes').
-- 
-- A typical usage example looks as follows:
-- 
-- > -- write 'match "posts/**.md"' instead of 'match $ fromGlob "posts/**.md"'
-- > {-# LANGUAGE OverloadedStrings #-}    
-- > ...
-- >
-- > main = hakyll $ do
-- >
-- >     -- Rule 1
-- >     -- Source files: all Markdown files like 'hakyll.md' in the 'posts' directory
-- >     match "posts/**.md" $ do          
-- >         -- Routing: Only replace extension, so '<destination-directory>/posts/hakyll.html'.
-- >         route $ setExtension "html"   
-- >         -- Compilation step(s): Transform Markdown file content into HTML output.
-- >         compile pandocCompiler        
-- >
-- >     -- Rule 2
-- >     match "css/*" $ do
-- >         route idRoute
-- >         compile compressCssCompiler
-- >     ...
-- __The order of rules doesn't matter.__ 
-- 
-- See official [Hakyll Rules tutorial](https://jaspervdj.be/hakyll/tutorials/03-rules-routes-compilers.html)
-- for other examples.
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
{- | Add a selection of which source files to process (using the
given [glob pattern](https://en.wikipedia.org/wiki/Glob_(programming\))) to the
given remaining 'Rules' value.

The expanded, relative path of the matched source file on disk (relative to the
project directory configured with 'Hakyll.Core.Configuration.providerDirectory')
becomes the identifier under which the compilation result is saved to the
'Hakyll.Core.Store.Store' (in case you want to 'Hakyll.Core.Compiler.load' it
within another rule).
See 'Hakyll.Core.Identifier.Identifier' for details.

=== __Examples__

__Select all markdown files within a directory (but without subdirectories)__

> -- Match all Markdown files in the immediate 'posts' directory
> -- e.g. '<project-directory>/posts/hakyll.md'
> -- but NOT  '<project-directory>/posts/haskell/monad.md'
> match "posts/*.md" $ do
>     route $ setExtension "html"
>     compile pandocCompiler

__Select all markdown files within a directory (including subdirectories recursively)__

> -- Match all Markdown files in the 'posts' directory and any subdirectory
> -- e.g. '<project-directory>/posts/hakyll.md'
> -- and  '<project-directory>/posts/haskell/monad.md'
> match "posts/**.md" $ do
>     route $ setExtension "html"
>     compile pandocCompiler
See 'Hakyll.Core.Identifier.Pattern.Pattern' or search "glob patterns" online
for more details. To control where the compilation result will be written out,
use routing functions like 'Hakyll.Core.Routes.setExtension'.
-}
match :: Pattern  -- ^ Glob pattern
      -> Rules () -- ^ Remaining processing parts
      -> Rules () -- ^ Result
match pattern = matchInternal pattern $ getMatches pattern


--------------------------------------------------------------------------------
{- | Add a selection of which source files to process (using the
given [glob pattern](https://en.wikipedia.org/wiki/Glob_(programming\)) and
metadata predicate) to the given remaining 'Rules' values. Same as 'match' but
allows to filter files further based on their (metadata) content (a file is
added only when the metadata predicate returns @True@).

The expanded, relative path of the matched source file on disk (relative to the
project directory configured with 'Hakyll.Core.Configuration.providerDirectory')
becomes the identifier under which the compilation result is saved to
the 'Hakyll.Core.Store.Store' (in case you want to 'Hakyll.Core.Compiler.load'
it within another rule).
See 'Hakyll.Core.Identifier.Identifier' for details.

=== __Examples__
__Select all markdown files with enabled draft flag within a directory__

> matchMetadata "posts/*.md" (\meta -> maybe False (=="true") $ lookupString "draft" meta) $ do
>     route $ setExtension "html"
>     compile pandocCompiler

For example, the following 'posts/hakyll.md' file with @draft: true@ metadata
would match:

> ---
> draft: true
> title: Hakyll Post
> ...
> ---
> In this blog post we learn about Hakyll ...
Note that files that have @draft: false@ or no such draft field at all, would
not match. You can use helper functions like 'Hakyll.Core.Metadata.lookupString'
to access a specific metadata field, and 'Data.Maybe.maybe' to work with
'Data.Maybe.Maybe'. To control where the compilation result will be written out,
use routing functions like 'Hakyll.Core.Routes.setExtension'.
-}
matchMetadata :: Pattern             -- ^ Glob pattern
              -> (Metadata -> Bool)  -- ^ Metadata predicate
              -> Rules ()            -- ^ Remaining processing parts
              -> Rules ()            -- ^ Result
matchMetadata pattern metadataPred = matchInternal pattern $
    map fst . filter (metadataPred . snd) <$> getAllMetadata pattern


--------------------------------------------------------------------------------
{- | Assign (and thereby create) the given identifier(s) to content that has no
underlying source file on disk. That content must be created within the
'compile' part of the given remaining 'Rules' value. The given identifier is the
id under which the compilation is saved to the 'Hakyll.Core.Store.Store' (in
case you want to 'Hakyll.Core.Compiler.load' it within another rule).
See 'Hakyll.Core.Identifier.Identifier' for details.

Use this function for example to create an overview page that doesn't have or
need its content prepared in a file (unlike blog posts which normally have a
corresponding Markdown source file on disk).

=== __Examples__
__Create a webpage without an underlying source file__

> -- saved with implicit identifier 'index.html' to Store
> create ["index.html"] $ do
>
>     -- compilation result is written to '<destination-directory>/index.html'
>     route idRoute
>
>     -- create content without a source file from disk
>     compile $ makeItem ("<h1>Hello World</h1>" :: String)
Note how you can use 'Hakyll.Core.Compiler.makeItem' to create content inline
(to be processed as a 'Hakyll.Core.Compiler.Compiler' value) as if that content
was loaded from a file (as it's the case when using 'match').
To control where the compilation result will be written out, use routing
functions like 'Hakyll.Core.Routes.idRoute'.
-}
create :: [Identifier] -- ^ Identifiers to assign to created content in next argument
       -> Rules ()     -- ^ Remaining processing parts that must create content
       -> Rules ()     -- ^ Resulting rule
create ids rules = do
    flush
    -- TODO Maybe check if the resources exist and call tellResources on that
    Rules $ local setMatches $ unRules $ rules >> flush
  where
    setMatches env = env {rulesMatches = ids}


--------------------------------------------------------------------------------
{- | Add the given version name to the implicit identifier(s) under which the
compilation result of the given remaining 'Rules' value is saved to the
'Hakyll.Core.Store.Store'.
See 'Hakyll.Core.Identifier.Identifier' for details.

Use this wrapper function for example when you need to compile the same source
file into two or more different results, each with a different version name.
The version is needed to distinguish between these different compilation results
in the store, otherwise they would get the same conflicting identifier in the
store.

Warning:
__If you add a version name with this function, you need to supply the same name__
when you 'Hakyll.Core.Compiler.load' the content from the store from
within another rule.

=== __Examples__
__Compile source file into differently versioned outputs and load both__

> -- e.g. file on disk: 'posts/hakyll.md'
>
> -- saved with implicit identifier ('posts/hakyll.md', no-version)
> match "posts/*" $ do
>     route $ setExtension "html"
>     compile pandocCompiler
>
> -- saved with implicit identifier ('posts/hakyll.md', version 'raw')
> match "posts/*" $ version "raw" $ do
>     route idRoute
>     compile getResourceBody
>
> -- use compilation results from rules above
> create ["index.html"] $ do
>     route idRoute
>     compile $ do
>         -- load no-version version
>         compiledPost <- load (fromFilePath "posts/hakyll.md")
>         -- load version 'raw'
>         rawPost <- load . setVersion (Just "raw") $ fromFilePath "posts/hakyll.md"
>         ...
Note how a version name is needed to distinguish the unversioned and the "raw"
version when loading the Hakyll post for the @index.html@ page.
To control where the compilation result will be written out, use routing
functions like 'Hakyll.Core.Routes.idRoute' and 'Hakyll.Core.Routes.setExtension'.
-}
version :: String   -- ^ Version name to add
        -> Rules () -- ^ Remaining processing parts
        -> Rules () -- ^ Result
version v rules = do
    flush
    Rules $ local setVersion' $ unRules $ rules >> flush
  where
    setVersion' env = env {rulesVersion = Just v}


--------------------------------------------------------------------------------
{- | Add (or replace) the given compilation steps within the given
'Hakyll.Core.Compiler.Compiler' value to the current 'Rules' value.
__This functions controls HOW the content within a rule is processed__ (use one
of the 'match' functions to control WHAT content is processed).

The compilation result is saved to the 'Hakyll.Core.Store.Store' under an
implicit identifier.
See 'Hakyll.Core.Identifier.Identifier' for details.

If there's routing attached to the rule where this function is used, the
compilation result is also written out to a file according to that route.
See 'route' and 'Hakyll.Core.Routes.Routes' for details.

=== __Examples__
__Compile Markdown to HTML__

> -- Select all Markdown files in 'posts' directory
> match "posts/**.md" $ do
>
>     route $ setExtension "html"
>
>     -- use pandoc to transform Markdown to HTML in a single step
>     compile pandocCompiler
Note how we set the content to be processed with
'Hakyll.Web.Pandoc.pandocCompiler'. The content comes implicitly from the
matched Markdown files on disk. We don't have to pass that content around
manually. Every file is processed the same way within this one rule.

To control where the compilation result will be written out, use routing
functions like 'Hakyll.Core.Routes.setExtension'.
Here the compilation result of a file like @posts\/hakyll.md@ is written out
to @posts\/hakyll.html@.

__Compile Markdown to HTML and embed it in a template__

> -- Select all Markdown files in 'posts' directory
> match "posts/**.md" $ do
>     route $ setExtension "html"
>     compile $
>         pandocCompiler >>=
>           loadAndApplyTemplate "templates/post.html" defaultContext
>
> -- To Hakyll templates are just plain files that have to be processed
> -- and placed into the store like any other file (but without routing).
> -- e.g. file on disk: 'templates/post.html'
> match "templates/*" $ compile templateBodyCompiler
Note how a Markdown post that is compiled to HTML using
'Hakyll.Web.Pandoc.pandocCompiler' in a first step and then embedded into
a HTMl 'Hakyll.Web.Template.Template' in a second step by using
'Hakyll.Web.Template.loadAndApplyTemplate'.
We can use templates to control the design and layout of a webpage.
A template may look as follows:

> <h1>$title$</h1>
> $body$
See "Hakyll.Web.Template" to see examples of the templating syntax.
-}
compile :: (Binary a, Typeable a, Writable a) => Compiler (Item a) -- ^ How to transform content
                                              -> Rules ()          -- ^ Result
compile compiler = Rules $ modify $ \s ->
    s {rulesCompiler = Just (fmap SomeItem compiler)}


--------------------------------------------------------------------------------
{- | Add (or replace) routing in the current 'Rules' value.
__This functions controls IF and WHERE the compiled results are written out__
(use one of the 'match' functions to control WHAT content is processed and
'compile' to control HOW).
See 'Hakyll.Core.Routes.Routes' and 'Hakyll.Core.Identifier.Identifier' for
details on how output filepaths are computed.

Hint:
__If there's no route attached to a rule, the compilation result is not written out__.
However, the compilation result saved to the 'Hakyll.Core.Store.Store'
and can be loaded and used within another rule. This behavior is needed,
for example, for templates.

=== __Examples__
__Rules with and without routing__

> -- e.g. file on disk: 'templates/post.html'
>
> -- Rule 1 (without routing)
> match "templates/*" $ do
>     -- compilation result saved to store with implicit identifier, e.g. 'templates/post.html'
>     compile templateCompiler
>
> -- Rule 2 (with routing)
> match "posts/**.md" $ do
>     route $ setExtension "html"
>     compile $ do
>        -- load compiled result of other rule with explicit identifier.
>        postTemplate <- loadBody "templates/post.html"
>        pandocCompiler >>= applyTemplate postTemplate defaultContext
Note that we don't set a route in the first rule to avoid writing out our
compiled templates (a webpage containing raw
'Hakyll.Web.Template.templateCompiler' templates makes rarely sense after all).
However, we can still 'Hakyll.Core.Compiler.load' (or
'Hakyll.Core.Compiler.loadBody') the compiled templates to apply them in a
second rule.
The content for 'Hakyll.Web.Template.templateCompiler' comes implicitly from the
matched template files on disk. We don't have to pass that content around
manually. See 'match' and 'compile' for details.

To control where a compilation result will be written out (as done in the second
rule), use routing functions like 'Hakyll.Core.Routes.setExtension'.

See "Hakyll.Web.Template" for examples of templates and the templating syntax.
-}
route :: Routes   -- ^ Where to output compilation results
      -> Rules () -- ^ Result
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
