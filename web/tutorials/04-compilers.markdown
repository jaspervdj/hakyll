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
about any of that: Hakyll will sort this out for you automatically!

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

Templates
---------

### Basic templates

Let's have a look at a simple template:

    <h1>$title$</h1>
    <div class="info">Posted on $date$</div>
    $body$

As you can probably guess, template files just contain text and only the `$`
character has special meaning: text between dollar signs ("fields") is replaced
when the template is applied. If you want an actual dollar sign in the output,
use `$$`.

You usually compile the templates from disk using the aptly named
`templateCompiler`:

    match "templates/*" $ compile templateCompiler

Notice the lack of `route` here: this is because we don't need to write the
templates to your `_site` folder, we just want to use them elsewhere.

### Templates: Context

We can easily guess the meaning of `$title$`, `$date$`, and `$body$`, but these
are not hard-coded fields: they belong to a certain [Context]. A `Context`
determines how the fields are interpreted. It's a [Monoid] and therefore very
composable.

[Context]: /reference/Hakyll-Web-Template-Context.html
[Monoid]: http://learnyouahaskell.com/functors-applicative-functors-and-monoids

`field` allows us to create a `Context` for a single field:

```haskell
field :: String -> (Item a -> Compiler String) -> Context a
```

Let's try this out. Note that this is for illustration purposes only: you
shouldn't have to write complicated fields often.  We can implement the `$body$`
field like this:

```haskell
field "body" $ \item -> return (itemBody item) :: Context String
```

And `$title$` like this:

```haskell
titleContext :: Context a
titleContext :: field "title" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "No title" $ M.lookup "title" metadata
```

And compose them using the `Monoid` instance:

```haskell
context :: Context String
context = mconcat
    [ titleContext
    , field "body" $ return . itemBody
    ]
```

Obviously, it would be tedious to implement things like `titleContext` over and
over again for different websites and different fields. This is why hakyll
provides `defaultContext`. `defaultContext` is a composed `Context` and allows
you to use:

- `$body$` for the body of the page;
- `$url$` for the destination URL of the page;
- `$path$` for the original filepath of the page;
- `$foo$` where foo is specified in the metadata.

`$date$` is not provided by default, you can see how we add it in the definition
of `postCtx` in `site.hs`:

```haskell
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
```

### Loading and applying templates

Now we know about templates, context and how to load arbitrary items. This gives
us enough background information in order to understand you can apply a
template:

```haskell
compile $ do
    tpl <- loadBody "templates/post.html"
    pandocCompiler >>= applyTemplate tpl postCtx
```

Loading and then immediately applying a template is so common there's a
shorthand function:

```haskell
compile $
    pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postCtx
```

## Producing a list of items

At this point, everything in the example site we generated should be clear to
you, except for how we produce the list of posts in `archive.html` and
`index.html`.

However, this really isn't hard and just uses the things we saw before: loading
other items and applying templates.

We can reproduce a list of items in the archive using the following code:

```haskell
compile $ do
    posts   <- recentFirst <$> loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    makeItem list
```

`recentFirst` sorts items by date. This relies on the convention that posts are
always named `YYYY-MM-DD-title.extension` in Hakyll -- if you use some other
format, you'll have to write some other sorting method.

```haskell
recentFirst :: [Item a] -> [Item a]
```

After loading and sorting the items, we load a template for the posts.
`applyTemplateList` applies this template to every post and concatenates the
result, which is a simple `String`. After that, we need `makeItem` to wrap the
returned `String` to `Item String`.