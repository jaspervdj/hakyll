---
title: Tutorial (Part I)
---

## Getting started

First, make sure you have Hakyll installed. The recommended way to do this is
through [hackage](http://hackage.haskell.org/) using
[cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install). This
tutorial also assumes you have a basic knowledge of Haskell.

~~~~~
[jasper@alice ~]$ cabal install hakyll
~~~~~

## Building a simple static site

As an example to get started with, we're going to develop a so called
"Brochure Site" for an imaginary company. The first step is to create
a directory for our new site.

~~~~~
[jasper@alice Sites]$ mkdir brochure
[jasper@alice Sites]$ cd brochure/
[jasper@alice brochure]$
~~~~~

I have a [zip file](examples/brochure.zip) with the files we need for this
tutorial available. Please unzip it in the brochure directory we just created.
We'll first have a look at what we're going to create (because we're curious
and all that).

~~~~~
[jasper@alice brochure]$ ghc --make hakyll.hs 
[1 of 1] Compiling Main             ( hakyll.hs, hakyll.o )
Linking hakyll ...
[jasper@alice brochure]$ ./hakyll preview
Generating...
Starting hakyll server on port 8000...
~~~~~

If you now point your browser at [localhost:8000](http://localhost:8000/) you
should see our simple brochure site.

## hakyll.hs

The main configuration file of a Hakyll site is traditionally called
`hakyll.hs`. It is nothing special, just a small Haskell program. There is no
magic going on.

~~~~~{.haskell}
import Text.Hakyll (hakyll)
main = hakyll $ do
    putStrLn "I'm in your computer, generating your site!"
~~~~~

Note how we wrap everyting in the `hakyll` function. This is useful because
it will generate a very nice main function.

## Pages

An important concept in Hakyll is Pages. Pages are text files that can be
written in markdown, html or TeX. Furthermore, they can also contain some
metadata. The metadata is placed in the file header and surrouded by `---`
lines. Each line should contain a `key: value` pair. Let's have a look at the
`about.markdown` page.

~~~~~{.markdown}
---
title: About
---
Nullam imperdiet sodales orci vitae molestie.
Nunc quam orci, pharetra a rhoncus vitae,
eleifend id felis. Suspendisse potenti...
~~~~~

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
          href="css/default.css" />
  </head>
  <body>
    <h1>MyAweSomeCompany - $title</h1>
    <div id="navigation">
      <a href="index.html">Home</a>
      <a href="about.html">About</a>
      <a href="products.html">Products</a>
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

## Putting it all together

Now, we'll render the page using the `renderChain` function. This function
takes a list of templates and a renderable object. In our case, we only have
one template, and our renderable object is simply a `PagePath`.

~~~~~{.haskell}
import Text.Hakyll (hakyll)
import Text.Render (renderChain)
import Text.Renderables (createPagePath)
main = hakyll $ do
    renderChain ["templates/default.html"]
        (createPagePath "about.markdown")
~~~~~

Or, to render all our three pages:

~~~~~{.haskell}
import Text.Hakyll (hakyll)
import Text.Render (renderChain)
import Text.Renderables (createPagePath)
main = hakyll $ do
    render "about.markdown"
    render "index.markdown"
    render "products.markdown"
    where render = renderChain ["templates/default.html"]
                 . createPagePath
~~~~~

This will create the following files:

~~~~~
_site/about.html
_site/index.html
_site/products.html
~~~~~

## CSS, images and other static files

Now, we also have a css file we would like to have in the `_site` directory.
Static files can be rendered using the `static` function in Hakyll. We could
use:

~~~~~{.haskell}
import Text.Hakyll (hakyll)
import Text.Render (static)
main = hakyll $ do
    static "css/default.css"
~~~~~

This would work, but let's not forget that Hakyll also has css compression. If
we want to use that, we would use `css` instead of `static`.

~~~~~{.haskell}
import Text.Hakyll (hakyll)
import Text.Render (css)
main = hakyll $ do
    css "css/default.css"
~~~~~

If we were to create another css file, we would have to add a line to our
`hakyll.hs` configuration file. This is pretty stupid, because the whole
directory `css` contains only css files. That's why Hakyll has a `directory`
function, which will execute a given function on an entire directory. So,
our example would become:

~~~~~{.haskell}
import Text.Hakyll (hakyll)
import Text.Render (css)
import Text.File (directory)
main = hakyll $ do
    directory css "css"
~~~~~

## Deploying

To setup your site, simply copy the contents of `_site` to your hosting provider
using your favorite piece of software.

## That's it!

Now you should fully understand our brochure site example. If you still have
questions, feel free to ask them on the
[google discussion group](http://groups.google.com/group/hakyll).

If you feel comfortable with the basics, here is a next tutorial:
[building a simple blog](tutorial2.html).
