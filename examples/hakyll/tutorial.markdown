---
title: Tutorial
---

Why static websites?
--------------------

Modern web frameworks make it easy to create huge dynamic websites. Why would
anyone still care about a static website?

- Static websites are fast, because it's simply files served directly from the
  hard disk.
- Static websites are secure. Nobody has even found an SQL injection in static
  pages.
- Static websites are easy to deploy. Just copy them to your webhost using
  (S)FTP/rsync/scp and you are done. They work on all webhosts: no CGI or extra
  modules needed for the web server.

Why Hakyll?
-----------

Hakyll is a [Haskell] library meant for creating small-to-medium sized static
websites. It is a powerful publishing tool, precisely because of the power of
Haskell. By using the awesome [pandoc] library, it is able to create your
website from a large variety of input formats.

[Haskell]: http://haskell.org/
[pandoc]:

Features include:

- easy templating system;
- a simple HTTP server for previewing and compiling your website on the go;
- powerful syntax highlighting;
- modules for common items such as tags and feeds;
- easily extensible.

A simple brochure site
----------------------

### The two layers

Hakyll consists of two important layers:

- A top-level declarative eDSL, used to describe the relations between the
  different items,
- A lower-level filter-like eDSL built on Arrows.

Both layer are used in the configuration file of your website. This
configuration file is conventionally called `hakyll.hs` and placed at the root
of your website directory.

### The Rules DSL

The Rules DSL is probably the simpler one. Let's look at a very simple example
of a `hakyll.hs`.  This piece of code might look a little confusing, but don't
worry, we'll walk through it in detail.

~~~~~{.haskell}
{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))
import Control.Monad (forM_)

import Hakyll

main :: IO ()
main = hakyll $ do
    route   "css/*" idRoute
    compile "css/*" compressCssCompiler

    compile "templates/*" templateCompiler

    forM_ ["about.rst", "index.markdown", "code.lhs"] $ \page -> do
        route   page $ setExtension "html"
        compile page $ readPageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
~~~~~

This is enough code to create a small brochure site! You can find all code
and files necessary to build this site [right here](TODO: add link!) -- feel
free to play around with it!

Our code begins with a number of imports. Nothing out of the ordinary here, but
do note that we use the `OverloadedStrings` extension for conciseness.

~~~~~{.haskell}
{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))
import Control.Monad (forM_)

import Hakyll
~~~~~

Our entry point is simply a `main` function with the type `IO ()` -- as in every
other Haskell application. However, we directly wrap it in a `hakyll` function,
which marks our declarative eDSL. Inside this function, we no longer operate in
the `IO` monad, we operate in the pure `RulesM` monad.

~~~~~{.haskell}
main :: IO ()
main = hakyll $ do
~~~~~

The `RulesM` monad is composed of a few functions. A first important one is
`route`: this creates a new rule for routing items. This rule is applied to all
items it matches -- and matching is done using the `"css/*"` [pattern].
`idRoute` simply means that an item will be routed to it's own filename. For
example, `css/screen.css` will be routed to `css/screen.css` -- not very
exciting.

Note that a [pattern] matches [identifiers], it doesn't match filenames.

[pattern]: /reference/Hakyll-Core-Identifier-Pattern.html
[identifiers]: /reference/Hakyll-Core-Identifier.html

~~~~~{.haskell}
route   "css/*" idRoute
~~~~~

Apart from specifying where the items should go (using `route`), we also have to
specify *how* the need to be compiled. This is done using the `compile`
function. As second argument, it takes a `Compiler`. These compilers can consist
of very complicated constructions, but Hakyll also provides a number of good
default compilers. The `compressCssCompiler` compiler will simply compress the
CSS found in the files.

~~~~~{.haskell}
compile "css/*" compressCssCompiler
~~~~~

Next, we're going to render some pages. We're going to style the results a
little, so we're going to need a [template]. We simply compile a template using
the `defaultTemplateRead` compiler, it's good enough in most cases.

[template]: /reference/Hakyll-Web-Template.html

We don't use a route for these templates, after all, we don't want to route them
anywhere, we just want to use them to style our pages a little.

~~~~~{.haskell}
compile "templates/*" templateCompiler
~~~~~

We can conclude that some rules do not *directly* add an output page on our
site. In this case, we compile the template so it is available to the compiler
later[^1].

[^1]: Actually, since the rules DSL is declarative, we could also add the
      template compile rule at the bottom -- this would make no difference.

Now, it's time to actually render our pages. We use the `forM_` monad combinator
so we can describe all files at once (instead of compiling all three files
manually).

~~~~~{.haskell}
forM_ ["about.rst", "index.markdown", "code.lhs"] $ \page -> do
~~~~~

The pages all have different extensions. In our website, we only want to see
`.html` files. Hakyll provides a route to do just that:

~~~~~{.haskell}
route   page $ setExtension "html"
~~~~~

The compilation of our pages is slightly more complicated: we're using another
DSL there.

### The Compiler DSL

The gist of it is that the `Compiler a b` type has two parameters -- it is an
Arrow, and we can chain compilers using the `>>>` operator. The [compiler]
reference page has some more readable information on this subject.

[compiler]: /reference/Hakyll-Core-Compiler.html

~~~~~{.haskell}
compile page $ pageCompiler
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler
~~~~~

Note that we can only use `applyTemplateCompiler` with
`"templates/default.html"` because we compiled `"templates/default.html"`. If we
didn't list a rule for that item, the compilation would fail (Hakyll would not
know what `"templates/default.html"` is!).
