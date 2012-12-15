---
title: More on compilers: load, and templates
author: Jasper Van der Jeugt
---

Loading items
-------------

The compiler Monad is a complex beast, but this is nicely hidden for the user of
the Hakyll library.

Suppose that you're generating `index.html` which shows your latest brilliant
blogpost. This requires `posts/foo.markdown` to be generated *before*
`index.html` (so we don't have to generate it twice). But you don't have to care
about all of that: Hakyll will sort this out this out for you automatically!

Let's see some quick examples. We can load a specific item:

```haskell
load "posts/foo.markdown" :: Compiler (Item String)
```

Or a whole bunch of them:

```haskell
loadAll "posts/*" :: Compiler [Item String]
```

Sometimes you just want the *contents* and not the `Item`:

```haskell
loadBody "posts/foo.markdown" :: Compiler String
```

This is all useful if we want to use Hakyll's templating system.

Basic templates
---------------
