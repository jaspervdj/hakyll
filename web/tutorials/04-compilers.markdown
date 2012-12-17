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

Basic templates
---------------

Let's have a look at a simple template:

    <h1>$title$</h1>
    <div class="info">Posted on $date$</div>
    $body$

As you can probably guess, template files just contain text and only the `$`
character has special meaning: text between dollar signs ("fields") is replaced
when the template is applied. If you want an actual dollar sign in the output,
use `$$`.

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

TODO: Write about defaultContext. Extend it with dateField.

You usually compile the templates from disk using the aptly named
`templateCompiler`:

    match "templates/*" $ compile templateCompiler

Notice the lack of `route` here: this is because we don't need to write the
templates to your `_site` folder, we just want to use them elsewhere.

Using them elsewhere is easy: we just use `load`!

TODO: Full example: load template, apply. Then `loadAndApplyTemplate`.

TODO: Load a list of posts, demonstrate `applyTemplateList`.
