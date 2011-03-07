---
title: Tutorial
---

Why static websites?
--------------------

Modern web frameworks make it easy to create huge dynamic websites. Why would
anyone still care about a static website?

- Static websites are fast, because it's simply files served directly from the
  hard disk.
- Static websites are secure. Nobody has ever found an SQL injection in static
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
[pandoc]: http://johnmacfarlane.net/pandoc/

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
        compile page $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
~~~~~

This is enough code to create a small brochure site! You can find all code
and files necessary to build this site [right here](/examples/brochure.zip)
-- feel free to play around with it!

To create your site, compile and run your `hakyll.hs`:

    [jasper@phoenix] ghc --make hakyll.hs
    [jasper@phoenix] ./hakyll preview

Alternatively,

    [jasper@phoenix] runghc hakyll.hs preview

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

Note that a [Pattern] matches an [Identifier], it doesn't match filenames.

[Pattern]: /reference/Hakyll-Core-Identifier-Pattern.html
[Identifier]: /reference/Hakyll-Core-Identifier.html

~~~~~{.haskell}
route   "css/*" idRoute
~~~~~

Apart from specifying where the items should go (using `route`), we also have to
specify *how* they need to be compiled. This is done using the `compile`
function. It takes a `Compiler` as its second argument. These compilers can
consist of very complicated constructions, but Hakyll also provides a number of
good default compilers. The `compressCssCompiler` compiler will simply compress
the CSS found in the files.

~~~~~{.haskell}
compile "css/*" compressCssCompiler
~~~~~

Next, we're going to render some pages. We're going to style the results a
little, so we're going to need a [Template]. We simply compile a template using
the `defaultTemplateRead` compiler, it's good enough in most cases.

[Template]: /reference/Hakyll-Web-Template.html

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

The [Rules] reference page has a complete listing of the API used.

[Rules]: /reference/Hakyll-Core-Rules.html

The compilation of our pages is slightly more complicated: we're using another
DSL there.

### The Compiler DSL

The gist of it is that the `Compiler a b` type has two parameters -- it is an
Arrow, and we can chain compilers using the `>>>` operator. The [Compiler]
reference page has some more readable information on this subject.

[Compiler]: /reference/Hakyll-Core-Compiler.html

~~~~~{.haskell}
compile page $ pageCompiler
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler
~~~~~

Note that we can only use `applyTemplateCompiler` with
`"templates/default.html"` because we compiled `"templates/default.html"`. If we
didn't list a rule for that item, the compilation would fail (Hakyll would not
find the template).

Now, let's look at the concrete compiler:

- `pageCompiler` starts by reading the [Page], parsing it, and rendering it
  using [pandoc].
- `applyTemplateCompiler` applies a [Template] which we have already loaded.
- `relativizeUrlsCompiler` will [relativize] the URL's so we have a site we can
  deploy everywhere.

[Page]: /reference/Hakyll-Web-Page.html
[relativize]: /reference/Hakyll-Web-RelativizeUrls.html

Various tips and tricks
-----------------------

### Syntax highlighting

Syntax highlighting is enabled by default in Hakyll. However, you also need to
enable it in pandoc. If no syntax highlighting shows up, try

    [jasper@phoenix] cabal install --reinstall -fhighlighting pandoc

### When to rebuild

If you execute a `./hakyll build`, Hakyll will build your site incrementally.
This means it will be very fast, but it will not pick up _all_ changes.

- In case you edited `hakyll.hs`, you first want to compile it again.
- It is generally recommended to do a `./hakyll rebuild` before you deploy your
  site.

After rebuilding your site, all files will look as "modified" to the filesystem.
This means that when you upload your site, it will usually transfer all files --
this can generate more traffic than necessary, since it is possible that some
files were not actually modified. If you use `rsync`, you can counter this using
the `--checksum` option.

### Using inotify for the preview server

Hakyll is able to use [inotify] to power the preview server. This is generally
faster and uses less resources than the default. However, [inotify] is only
supported on linux systems. You can enable the bindings using:

    [jasper@phoenix] cabal install -finotify hakyll

[inotify]: http://inotify.aiken.cz/

Problems
--------

### regex-pcre dependency on Mac OS X

Hakyll requires [regex-pcre], which might fail to build on Mac OS X. To solve
this problem, make sure the [pcre] C library is installed (via homebrew or
macports). Then install [regex-pcre] using:

    cabal install --extra-include-dirs=/usr/local/include regex-pcre

...and proceed to install Hakyll the regular way.

[regex-pcre]: http://hackage.haskell.org/package/regex-pcre
[pcre]: http://www.pcre.org/
