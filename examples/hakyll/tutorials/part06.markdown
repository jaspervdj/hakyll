---
title: Tags and manipulations
what: enhances our blog with tags and explains context manipulations.
---

## Context manipulations

Here, have [a zip file](examples/tagblog.zip) for this tutorial.

You probably remember that `Context` objects are just key-value mappings. We can
render those with templates, and then the `$key`'s in the template get
substituted by the appropriate values. This is a rather flexible system, but
there are limitations. Some of these limitations can be solved using
_context manipulations_.

Like rendering actions, _context manipulations_ are also simply
`HakyllAction Context Context` arrows. The `Text.Hakyll.ContextManipulations`
contains some functions to easily construct easy variants.

One of the most general functions is the `renderValue` function. Let's have a
look at it's type.

~~~~~{.haskell}
renderValue :: String
            -> String
            -> (String -> String)
            -> HakyllAction Context Context
~~~~~

This is the preferred way of creating context manipulations. The first argument
is the `key` to manipulate. The second argument is the `key` where the new value
should be placed. If this is the same as the first argument, it will be
replaced. The third argument is the function to manipulate the `value` with.

As a simple example, let's write a function that puts the `$title` in uppercase.

~~~~~{.haskell}
import Data.Char (toUpper)

titleUpper :: HakyllAction Context Context
titleUpper = renderValue "title" "title" $ map toUpper
~~~~~

Because the destination `key` is the same as the source `key`, we can also use
the `changeValue` function here.

~~~~~{.haskell}
titleUpper = changeValue "title" $ map toUpper
~~~~~

For further reading, refer to the `Text.Hakyll.ContextManipulations`
documentation.

## Applying Context Manipulations

Because we're dealing with Arrows again, we can use `>>>` to apply our
manipulations. For example, we could use or title manipulation like this:

~~~~~{.haskell}
renderChain ["templates/default.html"]
            (createPage "index.markdown" >>> titleUpper)
~~~~~

## Rendering dates

As you remember, in our previous blog, all posts had a file name like
`posts/yyyy-mm-dd-title.extension`, as is the Hakyll convention. But they also
had a metadata field `date`, containing a human-readable date. This is not very
D.R.Y., of course! Hakyll has a specialized `renderValue` function to deal with
dates encoded in paths: `renderDate`.

~~~~~{.haskell}
postManipulation :: HakyllAction Context Context
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

    ---
    title: A third post
    author: Publius Ovidius Naso
    tags: epic fail, ovidius
    ---
    Pellentesque tempor blandit elit, vel...

But first things first. We need to render a post list for every tag. We already
had some code to render a list of all posts. We're just going to abstract this
code into a more general function:

~~~~{.haskell}
renderPostList url title posts = do
    let list = createListingWith url ["templates/postitem.html"]
                                 posts [("title", Left title)]
    renderChain ["posts.html", "templates/default.html"] list
~~~~~

Our "render all posts" action can now be written as:

~~~~~{.haskell}
renderPostList "posts.html" "All posts" renderablePosts
~~~~~

## Tag links

We want to display the tags for our post under the title. But if we use the
`$tags` key in a template, we will just have the plain tags - they will not be
clickable. We can again solve this with a `ContextManipulation`. We have a
function that produces an url for a given tag:

~~~~~{.haskell}
tagToUrl tag = "$root/tags/" ++ removeSpaces tag ++ ".html"
~~~~~

`removeSpaces` is an auxiliary function from `Text.Hakyll.File`. Now, there is
a specialized `renderValue` function for creating linked tags called
`renderTagLinks`. This function simply takes a function that produces an url
for a given tag - the function we just wrote. Let's extend our
`postManipulation`.

~~~~~{.haskell}
postManipulation :: HakyllAction Context Context
postManipulation =   renderDate "date" "%B %e, %Y" "Unknown date"
                 >>> renderTagLinks tagToUrl
~~~~~

We apply this manipulation when we load the tags.

~~~~~{.haskell}
let renderablePosts =
        map ((>>> postManipulation) . createPage) postPaths
~~~~~

So, the `renderTagLinks` function replaces the `$tags` value from
`epic fail, random` to `<a href="$root/tags/epic-fail.html">epic fail</a>, ...`.
If we click a tag, we get a `404`. That's because we haven't generated the
post lists for every tag.

## The Tag Map

Hakyll provides a function called `readTagMap`. Let's inspect it's type.

~~~~~{.haskell}
type TagMap = Map String [HakyllAction () Context]
readTagMap String [FilePath] -> HakyllAction () TagMap
~~~~~

You give it a list of paths, and it creates a map that, for every tag, holds
a number of posts. We can easily use this to render a post list for every tag.
The first argument given is an "identifier", unique to this tag map. Hakyll
needs this so it can cache the tags.

~~~~~{.haskell}
let tagMap = readTagMap "postTags" postPaths
~~~~~

When we have the `TagMap`, we can need to render a post list for every tag.
There is a function in Hakyll designed more or less for this purpose:
`withTagMap`. This takes a `TagMap` and an action to execute for every tag and
it's associated posts. We pass a small function to it we create ourselves[^1]:

[^1]: Exercise for the reader: why do we use `>>> postManipulation` again here?

~~~~~{.haskell}
let renderListForTag tag posts =
        renderPostList (tagToUrl tag)
                       ("Posts tagged " ++ tag)
                       (map (>>> postManipulation) posts)
withTagMap tagMap renderPostList
~~~~~

There we go. We now have clickable tags, and a post list for every tag.

## A Tag Cloud

A tag cloud is a commonly found thing on blogs. Hakyll also provides code to
generate a tag cloud. Let's have a look at the `renderTagCloud` function.

~~~~~{.haskell}
renderTagCloud :: (String -> String)
               -> Float
               -> Float
               -> HakyllAction TagMap String
~~~~~

The first argument is, once again, a function to create an url for a given tag.
Then, we give a minimum and a maximum font size in percent, and we get a tag
cloud Arrow back. We can add this to our index:

~~~~~{.haskell}
let tagCloud = tagMap >>> renderTagCloud tagToUrl 100 200
    index = createListing "index.html"
                          ["templates/postitem.html"]
                          (take 3 renderablePosts)
                          [ ("title", Left "Home")
                          , ("tagcloud", Right tagCloud)
                          ]
renderChain ["index.html", "templates/default.html"] index
~~~~~

## The gist of it

- There's some handy, simple functions in `Text.Hakyll.ContextManipulations`.
- Seperate tags by commas and put them in the `$tags` field.
- Use `withTagMap` to render a list for every tag.
- Hakyll can also create tag clouds.
