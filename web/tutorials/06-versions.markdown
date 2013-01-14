---
title: Producing multiple versions of a single file
author: Jasper Van der Jeugt
---

Basics
------

Suppose that you want to make your writings available in raw markdown format, in
addition to the HTML available on your website. Is this possible using Hakyll?

In the [previous tutorial], we explained how you can use snapshots to store an
item while it's being processed. However, this does not allow you to route it to
a different location.

[previous tutorial]: /tutorials/05-snapshots-feeds.html

Instead, we must use `version` in order to do this. The type signature of
`version` does not reveal much:

```haskell
version :: String -> Rules () -> Rules ()
```

So let's look at an example:

```haskell
match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

match "posts/*" $ version "raw" $ do
    route   idRoute
    compile getResourceBody
```

Here, you can see how we produce two versions of each item in `posts/*`: one
`"raw"` version, and the default version.

Attention: patterns and versions
--------------------------------

However, there is one important thing to note: suppose you use `Pattern`s for a
function such as `loadAll`, e.g. to create an index page with all blogposts.

```haskell
loadAll "posts/*" :: Compiler [Item String]
```

is valid code, but **probably not** what you want to do: this will select all
`posts/*` items, meaning, both your HTML posts, and the raw versions. In order
to fix this, use any of the following:

```haskell
loadAll ("posts/*" .&&. hasNoVersion) :: Compiler [Item String]
```

for the default versions, i.e. the HTML pages, and:

```haskell
loadAll ("posts/*" .&&. hasVersion "raw") :: Compiler [Item String]
```

for the raw versions.
