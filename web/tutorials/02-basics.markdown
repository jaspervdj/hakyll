---
title: The basics
author: Jasper Van der Jeugt
---

Building and cleaning
---------------------

If you followed along with the previous tutorial, you should now have the
example site up and running. By running `./site build`, you created two
directories:

- `_site`, with your site as HTML files, ready to be deployed;
- `_cache`, which Hakyll uses internally.

`./site clean` removes these directories, and `./site rebuild` performs a
`clean` and then a `build`.

In general, it's only necessary to use `rebuild` when you made changes to your
`site.hs`, and not when you just made changes to the contents of your website.

Basic rules
-----------

Let's take a look at the `site.hs` file.

```haskell
main :: IO ()
main = hakyll $ do
    ...
```

Hakyll configurations are in the `Rules` monad. In order to run them, the
`hakyll` function is used, so your main function usually starts this way.
`hakyllWith` is also available, this function allows you specify a custom
[Configuration].

[Configuration]: /reference/Hakyll-Core-Configuration.html

Some actual rules look like this:

```haskell
match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

```

This is a declarative DSL: the order in which you write the rules make little
difference: Hakyll will use dependency tracking to determine the correct order.

We group the different rules using `match`. The first argument for `match` is a
[Pattern]. The `OverloadedStrings` extension allows us to just write `String`s
here, which are interpreted as globs --- all files in the `images/` directory,
and all files in the `css/` directory.

[Pattern]: /reference/Hakyll-Core-Identifier-Pattern.html

Basic routes
------------

The `route` function is used for determining the output file. For example, you
probably want to write the processed contents of `contact.markdown` to
`_site/contact.html` and not `_site/contact.markdown`.

`idRoute` is a commonly used and just keeps the filename. We use this for e.g.
the images and CSS files.

`setExtension` is another common route which takes a single argument: the
extension of the resulting file. In order to route `contact.markdown` to
`_site/contact.html`, use:

```haskell
route $ setExtension "html"
```

`customRoute` is a more advanced higher-order function which allows for even
more customization. You want to route `contact.markdown` to
`_site/nwodkram.tcatnoc`? No problem, just use:

```haskell
customRoute $ reverse . toFilePath
```

More information can be found in the [Routes] module.

[Routes]: /reference/Hakyll-Core-Routes.html

## Images

Let's start of with the `images/haskell-logo.png` file, because the processing
of this file is very simple: it is simply copied to the output directory. Let's
look at the relevant lines in the `hakyll.hs` file:

~~~~~{.haskell}
match "images/*" $ do
    route   idRoute
    compile copyFileCompiler
~~~~~

The first line specifies we will describe the process for compiling everything
in the `images/` folder: hakyll uses globs for this [^pattern].

[^pattern]: A little caveat is that these globs are not `String`s but
    `Pattern`s, so you need the `OverloadedStrings` extension.

We can see two simple rules next: [route] and [compile].

- [route] determines how the input file(s) get mapped to the output files.
  [route] only deals with file names -- not with the actual content!
- [compile], on the other hand, determines how the file content is processed.

[route]: /reference/Hakyll-Core-Rules.html#v:route
[compile]: /reference/Hakyll-Core-Rules.html#v:compile

In this case, we select the [idRoute]: which means the file name will be kept
the same (`_site` will always be prepended automatically). This explains the
name of [idRoute]: much like the `id` function in Haskell, it also maps values
to themselves.

[idRoute]: /reference/Hakyll-Core-Routes.html#v:idRoute

For our compiler, we use [copyFileCompiler], meaning that we don't process the
content at all, we just copy the file.

[copyFileCompiler]: /reference/Hakyll-Core-Writable-CopyFile.html#v:copyFileCompiler

## CSS

If we look at how the two CSS files are processed, we see something which looks
very familiar:

~~~~~{.haskell}
match "css/*" $ do
    route   idRoute
    compile compressCssCompiler
~~~~~

Indeed, the only difference with the images is that have now chosen for
[compressCssCompiler] -- a compiler which *does* process the content. Let's have
a quick look at the type of [compressCssCompiler]:

[compressCssCompiler]: /reference/Hakyll-Web-CompressCss.html#v:compressCssCompiler

~~~~~{.haskell}
compressCssCompiler :: Compiler Resource String
~~~~~

Intuitively, we can see this as a process which takes a `Resource` and produces
a `String`.

- A `Resource` is simply the Hakyll representation of an item -- usually just a
  file on the disk.
- The produced string is the processed CSS.

We can wonder what Hakyll does with the resulting `String`. Well, it simply
writes this to the file specified in the `route`! As you can see, routes and
compilers work together to produce your site.

## Templates

Next, we can see that the templates are compiled:

~~~~~{.haskell}
match "templates/*" $ compile templateCompiler
~~~~~

Let's start with the basics: what is a template? An example template gives us a
good impression:

~~~~~
<html>
    <head>
        <title>Hakyll Example - $$title$$</title>
    </head>
    <body>
        <h1>$$title$$</h1>

        $$body$$
    </body>
</html>
~~~~~

A template is a text file to lay out some content. The content it lays out is
called a page -- we'll see that in the next section. The syntax for templates is
intentionally very simplistic. You can bind some content by referencing the name
of the content *field* by using `$$field$$`, and that's it.

You might have noticed how we specify a compiler (`compile`), but we don't set
any `route`. Why is this?

We need to compile the template because we will need it later. If we compile a
page later using `templates/default.html`, Hakyll needs to know what
`templates/default.html` is. Note that we could move template compilation to the
bottom of our code. The order doesn't matter -- Hakyll will determine that for
you. But if you don't compile `templates/default.html` as a template, Hakyll
will not be able to take it into account when deciding the compilation order.

So, the `compile` needs to be there -- but why don't we set a `route` here?
Precisely because we don't want to our template to end up anywhere in our site
directory! We want to use it to lay out other items -- so we need to load
(compile) it, but we don't want to give it a real destination.

By using the `templates/*` pattern, we compile all templates in one go.

## Pages

The code for pages looks suspiciously more complicated:

~~~~~~{.haskell}
match (list ["about.rst", "index.markdown", "code.lhs"]) $ do
    route   $ setExtension "html"
    compile $ pageCompiler
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
~~~~~~

But we'll see shortly that this actually fairly straightforward. Let's begin by
exploring what a *page* is.

~~~~~~
---
title: Home
author: Jasper
---

So, I decided to create a site using Hakyll and...
~~~~~~

A page consists of two parts: a body, and metadata. As you can see above, the
syntax is not hard. The metadata part is completely optional, this is the same
page without metadata:

~~~~~~
So, I decided to create a site using Hakyll and...
~~~~~~

Hakyll supports a number of formats for the page body. Markdown, HTML and RST
are probably the most common. Hakyll will automatically guess the right format
if you use the right extension for your page.

~~~~~~{.haskell}
match (list ["about.rst", "index.markdown", "code.lhs"]) $ do
~~~~~~

We see a more complicated pattern here. Some sets of files cannot be described
easily by just one pattern, and here the [list] function can help us out. In
this case, we have three specific pages we want to compile.

[list]: /reference/Hakyll-Core-Identifier-Pattern.html#v:list

~~~~~~{.haskell}
route $ setExtension "html"
~~~~~~

For our pages, we do not want to use `idRoute` -- after all, we want to generate
`.html` files, not `.markdown` files or something similar! The [setExtension]
route allows you to simply replace the extension of an item, which is what we
want here.

[setExtension]: /reference/Hakyll-Core-Routes.html#v:setExtension

~~~~~~{.haskell}
compile $ pageCompiler
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler
~~~~~~

How should we process these pages? [pageCompiler] is the default compiler for
pages. [pageCompiler] does a few things:

- It parses the page into body and metadata
- It adds some extra metadata fields such as `$$url$$` and `$$path$$` (you
  shouldn't worry about these for now)
- It fill in possible `$$key$$`'s in it's own body
- It renders the page using pandoc

Which basically means that we end up with a `Page` that has the HTML content we
want as body. But we don't just want the plain content on our website -- we want
to decorate it with a template, for starters.

[pageCompiler]: /reference/Hakyll-Web-Page.html#v:pageCompiler

Different compilers can be chained in a pipeline-like way using Arrows. Arrows
form a complicated subject, but fortunately, most Hakyll users need not be
concerned with the details. If you are interested, you can find some information
on the [Understanding arrows] page -- but the only thing you really *need* to
know is that you can chain compilers using the `>>>` operator.

[Understanding arrows]: http://en.wikibooks.org/wiki/Haskell/Understanding_arrows

The `>>>` operator is a lot like a flipped function composition (`flip (.)`) in
Haskell, with the important difference that `>>>` is more general and works on
all Arrows -- including Hakyll compilers.

Here, we apply three compilers sequentially:

1. We load and render the page using `pageCompiler`
2. We apply the template we previously loaded using [applyTemplateCompiler]
3. We relativize the URL's on the page using [relativizeUrlsCompiler]

[applyTemplateCompiler]: /reference/Hakyll-Web-Template.html#v:applyTemplateCompiler
[relativizeUrlsCompiler]: /reference/Hakyll-Web-RelativizeUrls.html#v:relativizeUrlsCompiler

Relativizing URL's is a very handy feature. It means that we can just use
absolute URL's everywhere in our templates and code, e.g.:

~~~~~{.haskell}
<link rel="stylesheet" type="text/css" href="/css/default.css" />
~~~~~

Using the [relativizeUrlsCompiler], Hakyll will change this to:

~~~~~{.haskell}
<link rel="stylesheet" type="text/css" href="css/default.css" />
~~~~~

when we are compiling `index.html`, or

~~~~~{.haskell}
<link rel="stylesheet" type="text/css" href="../css/default.css" />
~~~~~

when we are compiling (some imaginary) `posts/foo.html`.  So Hakyll will
translate this to a relative URL for each page. This means we can host our site
at `example.com` and `example.com/subdir` without changing a single line of
code.

More tutorials are in the works...
