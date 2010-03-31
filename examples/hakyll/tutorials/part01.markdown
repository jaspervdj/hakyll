---
title: Quickstart
what: explains how to create a simple brochure site
---

## Getting started

First, make sure you have Hakyll installed. The recommended way to do this is
through [hackage] using [cabal-install]. This tutorial also assumes you have a
basic knowledge of Haskell.

[hackage]: http://hackage.haskell.org/
[cabal-install]: http://www.haskell.org/haskellwiki/Cabal-Install

~~~~~
[jasper@alice ~]$ cabal install hakyll
~~~~~

## Building a simple static site

As an example to get started with, we're going to develop a so called
"Brochure Site" for an imaginary company. The first step is to create a
directory for our new site.

~~~~~
[jasper@alice Sites]$ mkdir brochure
[jasper@alice Sites]$ cd brochure/
[jasper@alice brochure]$
~~~~~

I have a [zip file] with the files we need for this
tutorial available. Please unzip it in the brochure directory we just created.
We'll first have a look at what we're going to create (because we're curious
and all that).

[zip file]: $root/examples/brochure.zip

~~~~~
[jasper@alice brochure]$ ghc --make hakyll.hs 
[1 of 1] Compiling Main             ( hakyll.hs, hakyll.o )
Linking hakyll ...
[jasper@alice brochure]$ ./hakyll preview
Starting hakyll server on port 8000...
~~~~~

If you now point your browser at [localhost:8000] you should see our simple
brochure site.

[localhost:8000]: http://localhost:8000/

## hakyll.hs

The main configuration file of a Hakyll site is traditionally called
`hakyll.hs`. It is nothing special, just a small Haskell program. There is no
magic going on.

~~~~~{.haskell}
import Text.Hakyll (hakyll)
main = hakyll "http://example.com" $ do
    liftIO $ putStrLn "I'm in your computer, generating your site!"
~~~~~

Note how we wrap everything in the `hakyll` function. This is useful because
it will generate a very nice main function. We also pass the full site URL to
the `hakyll` function. If you don't have an URL for your site yet, it doesn't
really matter for now; just fill in anything then. The URL is only used for
certain specific purposes where a full URL is needed, such as rendering RSS
feeds.

## Context

Let's look at one of the most important types in Hakyll.

~~~~~{.haskell}
type Context = Map String String
~~~~~

A `Context` is a key-value mapping, used to represent pieces of information.
One way to write such a `Context`, is a page.

## Pages

Another important concept in Hakyll is pages. Pages are text files that can be
written in markdown, html, rst... basically anything Pandoc supports.
Furthermore, they can also contain some metadata. The metadata is placed in the
file header and surrounded by `---` lines. Each line should contain a
`key: value` pair. Let's have a look at the `index.markdown` page.

    ---
    title: About
    ---
    Nullam imperdiet sodales orci vitae molestie.
    Nunc quam orci, pharetra a rhoncus vitae,
    eleifend id felis. Suspendisse potenti...

This contains one `key: value` pair, namely `title: About`. The rest of the
file is treated as markdown by pandoc. If you want to know more about
markdown, I think [this](http://daringfireball.net/projects/markdown/syntax)
is a pretty good page.

## Templates

Another concept are the so-called templates. Templates are text files (usually
html files) containing a number of keys. The syntax for these keys is
`$identifier`. Our example site contains one template, namely
`templates/default.html`. Let's have a better look at that.

~~~~~{.html}
<html>
  <head>
    <title>MyAweSomeCompany - $title</title>
    <link rel="stylesheet" type="text/css"
          href="$$root/css/default.css" />
    <link rel="stylesheet" type="text/css"
          href="$$root/css/syntax.css" />
  </head>
  <body>
    <h1>MyAweSomeCompany - $title</h1>
    <div id="navigation">
      <a href="$$root/index.html">Home</a>
      <a href="$$root/about.html">About</a>
      <a href="$$root/code.html">Code</a>
    </div>

    $body
  </body>
</html>
~~~~~

We can see how our `Page` would fit in. When we render the page we saw using
this template, `$title` would be replaced by `About`, and `$body` would be
replaced by the body of the about page. `body` is the traditional name for the
body of any page - that is the convention in Hakyll. Also note that in this
case, `$body` would be replaced by a chunk of html - the result of the
markdown-to-html conversion.

## The $$root key

There are a few "special" keys in Hakyll: one of them is the $$root key. What 
is so special about it? Well, internally, it is treated differently - but this 
should not concern you. The thing is that it is the only key you can also use 
in __Pages__.

It will be substituted by a relative url part (like `..` or `../..`) so it
points to the root directory of your site. It is recommended to use this
whenever you need it, it can save you some time from messing with absolute
and relative URL's.

## Putting it all together

Now, we'll render the page using the `renderChain` function. This function
takes a list of templates and a `Context`. In our case, we only have one
template, and our `Context` is the about page we just saw - we can load that
using the `createPage` function.

~~~~~{.haskell}
import Text.Hakyll (hakyll)
import Text.Hakyll.Render (renderChain)
import Text.Hakyll.CreateContext (createPage)
main = hakyll "http://example.com" $ do
    renderChain ["templates/default.html"]
        (createPage "index.markdown")
~~~~~

Or, to render all our three pages:

~~~~~{.haskell}
import Text.Hakyll (hakyll)
import Text.Hakyll.Render (renderChain)
import Text.Hakyll.CreateContext (createPage)
main = hakyll "http://example.com" $ do
    render "about.rst"
    render "index.markdown"
    render "code.lhs"
    where render = renderChain ["templates/default.html"]
                 . createPage
~~~~~

As you can see, we can render a variety of formats. This will create the
following files:

~~~~~
_site/about.html
_site/index.html
_site/code.html
~~~~~

## CSS, images and other static files

Now, we also have a css file we would like to have in the `_site` directory.
Static files can be rendered using the `static` function in Hakyll. We could
use:

~~~~~{.haskell}
import Text.Hakyll (hakyll)
import Text.Hakyll.Render (static)
main = hakyll "http://example.com" $ do
    static "css/default.css"
~~~~~

This would work, but let's not forget that Hakyll also has css compression. If
we want to use that, we would use `css` instead of `static`.

~~~~~{.haskell}
import Text.Hakyll (hakyll)
import Text.Hakyll.Render (css)
main = hakyll "http://example.com" $ do
    css "css/default.css"
~~~~~

If we were to create another css file, we would have to add a line to our
`hakyll.hs` configuration file. This is pretty stupid, because the whole
directory `css` contains only css files. That's why Hakyll has a `directory`
function, which will execute a given function on an entire directory. So,
our example would become:

~~~~~{.haskell}
import Text.Hakyll (hakyll)
import Text.Hakyll.Render (css)
import Text.Hakyll.File (directory)
main = hakyll "http://example.com" $ do
    directory css "css"
~~~~~

## Deploying

To setup your site, simply copy the contents of `_site` to your hosting provider
using your favorite piece of software.

## The gist of it

- You render "pages" with "templates".
- The most common render function is `renderChain`.
- Hakyll also deals with static files and css.
