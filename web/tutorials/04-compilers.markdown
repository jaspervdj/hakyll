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

```html
<h1>$title$</h1>
<div class="info">Posted on $date$</div>
$body$
```

As you can probably guess, template files just contain text and only the `$`
character has special meaning: text between dollar signs ("fields") is replaced
when the template is applied. If you want an actual dollar sign in the output,
use `$$`.

You usually compile the templates from disk using the aptly named
`templateCompiler`:

```haskell
match "templates/*" $ compile templateCompiler
```

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
titleContext = field "title" $ \item -> do
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

Control flow in templates
-------------------------

Sometimes string interpolation does not suffice, and you want a little more
control over how your templates are layed out. Hakyll provides a few control
structures for this. The syntax for these structures was based on the syntax
used in pandoc templates, since Hakyll already has tight integration with
pandoc.

### Conditionals

In `templates/post.html` of the example site we generated using `hakyll-init`,
we see an example of a conditional:

```html
<div class="info">
    Posted on $date$
    $if(author)$
        by $author$
    $endif$
</div>
```

This example should be pretty straightforward. One important thing to notice is
that `$if(foo)$` **does not** check the truth value of `$foo$`: it merely checks
if such a key is present.

Note that an if-else form is supported as well:

```html
<div class="info">
    Posted on $date$
    $if(author)$
        by $author$
    $else$
        by some unknown author
    $endif$
</div>
```

### Partials

Partials allow you to [DRY] up your templates by putting repetitive actions into
separate template files. You can then include them using
`$partial("filename.html")$`.

[DRY]: http://en.wikipedia.org/wiki/Don%27t_repeat_yourself

An example can be found in `templates/archive.html`:

```html
Here you can find all my previous posts:
$partial("templates/post-list.html")$
```

This partial is just another template and uses the same syntax. Note that in
order to use something like this, we also need to load the partial template in
our `site.hs`:

```haskell
match "templates/post-list.html" $ compile templateCompiler
```

Fortunately, we usually don't need to add this since we already have:

```haskell
match "templates/*" $ compile templateCompiler
```

### Producing a list of items: for

At this point, everything in the example site we generated should be clear to
you, except for how we produce the list of posts in `archive.html` and
`index.html`. Let's look at the `templates/post-list.html` template:

```html
<ul>
    $for(posts)$
        <li>
            <a href="$url$">$title$</a> - $date$
        </li>
    $endfor$
</ul>
```

This uses the `$for(foo)$` construct. This construct allows you loop over a
list, in this case, `$posts$`. Inside the body of this for loop, all fields
refer to the current post, e.g.: `$url$`, `$title$` and `$date$`.

Of course, $posts$ does not magically appear. We have to specify this in
`site.hs`. Let's look at how `archive.html` is generated:

```haskell
posts <- recentFirst =<< loadAll "posts/*"
let archiveCtx =
        listField "posts" postCtx (return posts) `mappend`
        constField "title" "Archives"            `mappend`
        defaultContext
```

We discussed `loadAll` earlier in this tutorial.

`recentFirst` sorts items by date. This relies on the convention that posts are
always named `YYYY-MM-DD-title.extension` in Hakyll -- or that the date must be
present in the metadata.

```haskell
recentFirst :: [Item a] -> Compiler [Item a]
```

After loading and sorting the items, we use `listField` to create the `$posts$`
key.

```haskell
listField :: String -> Context a -> Compiler [Item a] -> Context b
```

The first parameter is simply the name of the key (`"posts"`). Secondly we have
a `Context` with which all items should be rendered -- for our example site, we
already wrote such a `Context` for posts: `postCtx`. Lastly, we have a
`Compiler` which loads the items. We already loaded the items so we can just use
`return posts`.

The following snippet would produce the same result:

```haskell
let archiveCtx =
        listField "posts" postCtx (recentFirst =<< loadAll "posts/*") `mappend`
        constField "title" "Archives"                                 `mappend`
        defaultContext
```
