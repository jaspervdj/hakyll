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

Basic compilers
---------------

More information can be found in the [Routes] module.

[Routes]: /reference/Hakyll-Core-Routes.html

The `compile` function determines how the content is produced for a certain
item. `compile` takes a value of the type `Compiler (Item a)`. Let's look at
some common examples:

- `copyFileCompiler` is self-explanatory, the output is exactly the same as the
  input;
- `compressCssCompiler` performs some simple build-in compression
  transformations for CSS;
- `pandocCompiler` reads markdown, reStructuredText, or another input format and
  renders it as HTML (if you want to pass specific options to pandoc, use
  `pandocCompilerWith`).

Compilers are very flexible: `Compiler` is a [Monad] and `Item` is a [Functor].

[Monad]: http://learnyouahaskell.com/a-fistful-of-monads
[Functor]: http://learnyouahaskell.com/functors-applicative-functors-and-monoids

A good example to illustrate the `Monad` instance for `Compiler` is

```haskell
relativizeUrls :: Item String -> Compiler (Item String)
```

This compiler traverses your HTML and changes absolute URLs (e.g.
`/posts/foo.markdown` into relative ones: `../posts/foo.markdown`). This is
extremely useful if you want to deploy your site in a subdirectory (e.g.
`jaspervdj.be/hakyll` instead of `jaspervdj.be`). Combining this with the
`pandocCompiler` gives us:

```haskell
pandocCompiler >>= relativizeUrls :: Compiler (Item String)
```

For a real website, you probably also want to use templates in order to give
your pages produced by pandoc a nice layout. We tackle this in the next
tutorial.
