---
title: Tutorial (Part IV)
what: adds tags and context manipulations to our blog
---

Here, have [a zip file](examples/tagblog.zip) for this tutorial.

## Context manipulations

As you might remember, `Renderable` objects are usually just key-value mappings.
We can render those with templates, and then the `$key`'s in the template get
substituted by the appropriate values. This is a rather flexible system, but
there are limitations. Some of these limitations can be solved using
_Context Manipulations_.

In `Text.Hakyll.Context`, we see the type of `ContextManipulation`:

~~~~~{.haskell}
type ContextManipulation = Context -> Context
~~~~~

Where `Context` is a key-value mapping. Usually, you don't create
`ContextManipulation`'s by hand, but you use the `renderValue` function. Let's
have a look at it's type.

~~~~~{.haskell}
renderValue :: String -> String
            -> (String -> String)
            -> ContextManipulation
~~~~~

This is the preferred way of creating context manipulations. The first argument
is the `key` to manipulate. The second argument is the `key` where the new value
should be placed. If this is the same as the first argument, it will be
replaced. The third argument is the function to manipulate the `value` with.

As a simple example, let's write a function that puts the `$title` in uppercase.

~~~~~{.haskell}
import Data.Char (toUpper)

titleUpper :: ContextManipulation
titleUpper = renderValue "title" "title" $ map toUpper
~~~~~

## Applying Context Manipulations

Now, the question is how to apply these `ContextManipulation`'s. The answer is
simple. For every important render function (`render`, `renderChain`,
`renderAndConcat`), there is a variant that takes a `ContextManipulation` as a
first argument. These functions are thus: `renderWith`, `renderChainWith`,
`renderAndConcatWith`. In fact, the following holds true:

~~~~~{.haskell}
render == renderWith id
renderChain == renderChainWith id
renderAndConcat == renderAndConcatWith id
~~~~~

So we could use or title manipulation like this:

~~~~~{.haskell}
renderChainWith titleUpper ["templates/default.html"]
                (createPagePath "index.markdown")
~~~~~

## Rendering dates

As you remember, in our previous blog, all posts had a file name like
`posts/yyyy-mm-dd-title.extension`, as is the Hakyll convention. But they also
had a metadata field `date`, containing a human-readable date. This is not very
D.R.Y., of course! Hakyll has a specialized `renderValue` function to deal with
dates encoded in paths: `renderDate`.

~~~~~{.haskell}
postManipulation :: ContextManipulation
postManipulation = renderDate "date" "%B %e, %Y" "Unknown date"
~~~~~

That manipulation will:
- Read the date from the file name the post was loaded from.
- Parse the date and render it in a `%B %e, %Y` format. This is a
  `Month day, Year` format.
- Put the result in the `date` metadata field.
- If the date could not be parsed, it will put `"Unknown date"` in the `date`
  metadata field.

So, we can throw away our `date: ` lines from our posts, and still use `$date`
in our templates.

## Abstracting the post list

Now, we're going to render tags. This is also done using context manipulations.
Hakyll has a specialized module to deal with tags, provided by
`Text.Hakyll.Tags`. This module assumes tags are comma separated, and placed in
the `tags` metadata field.

> ---
> title: A third post
> author: Publius Ovidius Naso
> tags: epic fail, ovidius
> ---
> Pellentesque tempor blandit elit, vel...

But first things first. We need to render a post list for every tag. We already
had some code to render a list of all posts. We're just going to abstract this
code into a more general function:

~~~~{.haskell}
renderPostList url title posts = do
    let postItems = renderAndConcatWith
                        postManipulation
                        "templates/postitem.html"
                        (map createPagePath posts)
        customPage = createCustomPage
                            url
                            ("templates/postitem.html" : posts)
                            [ ("title", Left title)
                            , ("posts", Right postItems)
                            ]
    renderChain ["posts.html", "templates/default.html"]
                customPage
~~~~~

Our "render all posts" action can now be written as:

~~~~~{.haskell}
renderPostList "posts.html" "All posts" postPaths
~~~~~

## Tag links

We want to display the tags for our post under the title. But if we use the
`$tags` key in a template, we will just have the plain tags - they will not be
clickable. We can again solve this with a `ContextManipulation`. We have a
function that produces an url for a given tag:

~~~~~{.haskell}
tagToURL tag = "/tags/" ++ removeSpaces tag ++ ".html"
~~~~~

`removeSpaces` is an auxiliary function from `Text.Hakyll.File`. Now, there is
a specialized `renderValue` function for creating linked tags called
`renderTagLinks`. This function simply takes a function that produces an url
for a given tag - the function we just wrote. Let's extend our
`postManipulation`.

~~~~~{.haskell}
postManipulation :: ContextManipulation
postManipulation = renderDate "date" "%B %e, %Y" "Unknown date"
                 . renderTagLinks tagToURL
~~~~~

If we click a tag, we get a `404`. That's because we haven't generated the
post lists for every tag.

## The Tag Map

Hakyll provides a function called `readTagMap`. Let's inspect it's type.

~~~~~{.haskell}
readTagMap [FilePath] -> IO Map String [FilePath]
~~~~~

You give it a list of paths, and it creates a map that, for every tag, holds
a number of posts. We can easily use this to render a post list for every tag.

~~~~~{.haskell}
tagMap <- readTagMap postPaths
let renderListForTag (tag, posts) =
        renderPostList (tagToURL tag)
                       ("Posts tagged " ++ tag)
mapM_ renderListForTag (toList tagMap)
~~~~~

There we go. We now have clickable tags, and a post list for every tag.

## A Tag Cloud

A tag cloud is a commonly found thing on blogs. Hakyll also provides code to
generate a tag cloud. Let's have a look at the `renderTagCloud` function.

~~~~~{.haskell}
TagCloud :: M.Map String [FilePath]
         -> (String -> String)
         -> Float
         -> Float
         -> String
~~~~~

The first argument is obviously the result of the `readTagMap` function. The
second argument is, once again, a function to create an url for a given tag.
Then, we give a minimum and a maximum font size in percent, and we get the
tag cloud back. We can add this to our index:

~~~~~{.haskell}
let tagCloud = renderTagCloud tagMap tagToURL 100 200
...
createCustomPage "index.html"
                ("templates/postitem.html" : take 3 postPaths)
                [ ("title", Left "Home")
                , ("posts", Right recentPosts)
                , ("tagcloud", Left tagCloud)
                ]
~~~~~

## That's it

Feel free to hack around with the code from the zip file. As always, if you
still have questions, ask them at the
[google discussion group](http://groups.google.com/group/hakyll).
