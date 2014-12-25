---
title: Rules, routes and compilers
author: Jasper Van der Jeugt
---

Basic rules
-----------

While changing the content is nice, you'll have time for that once your site is
configured. Configuration is done using the `site.hs` file: let's take a look at
it!

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

This is a declarative DSL: the order in which you write the rules makes little
difference, Hakyll will use dependency tracking to determine the correct order.

We group the different rules using `match`. The first argument for `match` is a
[Pattern]. The `OverloadedStrings` extension allows us to just write `String`s
here, which are interpreted as globs --- all files in the `images/` directory,
and all files in the `css/` directory.

[Pattern]: /reference/Hakyll-Core-Identifier-Pattern.html

However, we can see that one item makes no use of `match`, but uses `create`
instead.

```haskell
create ["archive.html"] $ do
    route idRoute
    compile $ do
        ...
```

Don't pay attention to the somewhat complicated-looking stuff in `compile` --
this will become clear soon. The real question here is why we use `create`
instead of `match`.

The answer is simple: there is no `archive.html` file in our project directory!
So if we were to use `match`, no file would be matched, and hence, nothing
would appear in the output directory. `create`, however, ensures the items
listed are always produced.

Basic routes
------------

The `route` function is used for determining the output file. For example, you
probably want to write the processed contents of `contact.markdown` to
`_site/contact.html` and not `_site/contact.markdown`.

`idRoute` is a commonly used route and just keeps the filename. We use this for
e.g.  the images and CSS files.

`setExtension` is another common route which takes a single argument: the
desired extension of the resulting file. In order to route `contact.markdown` to
`_site/contact.html`, use:

```haskell
route $ setExtension "html"
```

`customRoute` is a more advanced higher-order function which allows for even
more customization. You want to route `contact.markdown` to
`_site/nwodkram.tcatnoc`? No problem, just use:

```haskell
route $ customRoute $ reverse . toFilePath
```

More information can be found in the [Routes] module.

[Routes]: /reference/Hakyll-Core-Routes.html

Basic compilers
---------------

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
