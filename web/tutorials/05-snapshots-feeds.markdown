---
title: Snapshots, and how to produce an RSS/Atom feed
author: Jasper Van der Jeugt
---

Basic feed configuration
------------------------

Hakyll has built-in support for two types of feeds: RSS and Atom. This tutorial
explains how you can add these to your blog or website. The first step is to
define a `FeedConfiguration` to set some basic options. For example, a cooking
blog may have the following declaration:

```haskell
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Healthy cooking: latest recipes"
    , feedDescription = "This feed provides fresh recipes for fresh food!"
    , feedAuthorName  = "John Doe"
    , feedAuthorEmail = "test@example.com"
    , feedRoot        = "http://healthycooking.example.com"
    }
```

Simple feed rendering
---------------------

Now, let's look at how we actually create a feed. Two functions are available:

```haskell
renderAtom :: FeedConfiguration
           -> Context String
           -> [Item String]
           -> Compiler (Item String)
```

```haskell
renderRss :: FeedConfiguration
          -> Context String
          -> [Item String]
          -> Compiler (Item String)
```

As you can see, they have exactly the same signature: we're going to use
`renderAtom` in this tutorial, but it's trivial to change this to an RSS feed.

```haskell
create ["atom.xml"] $ do
    route idRoute
    compile $ do
        let feedCtx = postCtx `mappend`
                constField "description" "This is the post description"

        posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
        renderAtom myFeedConfiguration feedCtx posts
```

There we go! We simply take the 10 last posts and pass them to `renderAtom`,
with our configuration and a `Context`.

It's a bit of a problem that we don't have a description for our posts, and the
Atom/RSS feed renderer requires this. One option is to add a `description: Foo`
header to our all posts. However, the description is the body text as it appears
in most RSS readers, so we would prefer to include the entire content of the
posts here.

Snapshots
---------

This poses a problem: if we just `load` all posts and take their content: we get
the *finished, processed* content. This means all templates have been applied at
that point (including `templates/default.html`). We don't want to include our
entire site with navigation in the RSS feed, but rather just the post HTML.

Snapshots provide a solution to this problem. They allow you to save an `Item`
at any point during its compilation, so you can `load` it later. Let's apply
this to our concrete problem.

```haskell
match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
```

now becomes:

```haskell
match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
```

The `saveSnapshot` function is really simple: it takes an item and returns the
same item, after saving it. This return value makes it easier to use
`saveSnapshot` in a chain of compilers as we did in the above example, but you
can discard it if you want.

```haskell
type Snapshot = String

saveSnapshot :: (Typeable a, Binary a)
             => Snapshot -> Item a -> Compiler (Item a)
```

Including the post body
-----------------------

With this modification, we can update our Atom code. Instead of loading the
compiled posts, we just load their content (i.e. the snapshot we just took).

We update the `Context` to map `$description$` to the post body, and we're done!

```haskell
create ["atom.xml"] $ do
    route idRoute
    compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots "posts/*" "content"
        renderAtom myFeedConfiguration feedCtx posts
```
