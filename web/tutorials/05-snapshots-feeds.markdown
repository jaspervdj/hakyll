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

Now, we'll create the actual feed.

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
