---
title: Creating a Blog
what: creates a simple blog
---

## Creating a simple blog with Hakyll

After we created a simple brochure site, we're going to try something more
advanced: we are going to create a simple blog system.

A [zip file] containing the source for this tutorial is also available.

[zip file]: $root/examples/simpleblog.zip

Blogs, as you probably know, are composed of posts. In Hakyll, we're going
to use simple pages for posts. All posts are located in the `posts`
directory. But we're not going to use the `directory` command here - you will
see why later. First, some trivial things like css.

~~~~~{.haskell}
main = hakyll "http://example.com" $ do
    directory css "css"
~~~~~

## Finding the posts

`Text.Hakyll.File` contains a handy function `getRecursiveContents`, which will
provide us with all the blog posts. The blog posts have a
`yyyy-mm-dd-title.extension` naming scheme. This is just a simple trick so we
can sort them easily (sorting on filename implies sorting on date). You could of
course name them whatever you want, but it's always a good idea to stick to the
conventions. They contain some metadata, too:

    title: A first post
    author: Julius Caesar
    date: November 5, 2009
    ---
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
    Vivamus pretium leo adipiscing lectus iaculis lobortis.
    Vivamus scelerisque velit dignissim metus...

Now, we find the posts and sort them reversed, so the most recent post will
become the first item in the list:

~~~~~{.haskell}
postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
~~~~~

Our `postPaths` value is now of the type `[FilePath]`. We want to be able to
render all posts, so we pass them to the `createPage` function.

~~~~~{.haskell}
let postPages = map createPage postPaths
~~~~~

We have two templates we want to render our posts with: first we would like to
render them using `templates/post.html`, and we want to render the result
using `templates/default.html`. This can be done with the `renderChain`
function:

~~~~~{.haskell}
mapM_ (renderChain [ "templates/post.html"
                   , "templates/default.html"
                   ]) postPages
~~~~~

Remember that the `renderChain` works by rendering the item using the first
template, creating a new page with the render result in the `$body` field, and
so on until it has been rendered with all templates.

Now, we have the posts rendered. What is left is to generate some kind of index
page with links to those posts. We want one general list showing all posts, and
we want to show a few recent posts on the index page.

## Creating listings.

`createPage` is the easiest way of reading a `Context`. But in this case, we
want something more custom, so we'll use the `createCustomPage` function. This
allows us to create a more specific `Context`.

~~~~~{.haskell}
createCustomPage :: FilePath
                 -> [(String, Either String (HakyllAction () String))]
                 -> HakyllAction () Context
~~~~~

The first argument is the `url` of the page to generate. For our index page,
this will be, `index.html`. The second argument is obviously our `key: value`
mapping. But why the `Either`? This, once again, is about dependency handling.
The idea is that you can choose which type to use for the value:

- `String`: Simply a `String`.
- `HakyllAction () String`: Here, you can give an `HakyllAction` Arrow action
  that can produce a String. However - this action _will not be executed_ when
  the file in `_site` is up-to-date.

However, in this specific case - a list of posts - there is an easier, and more
high-level approach than `createCustomPage`[^1]. Let's look at the type
signature of `createListing`:

~~~~~{.haskell}
createListing :: FilePath
              -> [FilePath]
              -> [HakyllAction () Context]
              -> [(String, Either String (HakyllAction () String))]
              -> HakyllAction () Context
~~~~~

[^1]: Since Hakyll-1.3 onwards.

The first argument is the destination url. For our blog, this is of course
`index.html`. The second argument is a list templates to render _each_ `Context`
with. We use only `templates/postitem.html` here. This is, as you can see, a
simple template:

~~~~~{.html}
<li>
    <a href="$$root/$url">$title</a>
    - <em>$date</em> - by <em>$author</em>
</li>
~~~~~

We then give a list of `Context`s to render. For our index, these are the 3 last
posts.  The last argument of the `createListing` functions lets you specify
additional key-value pairs, like in `createCustomPage`. We use this to set the
title of our page. So, we create our index page using:

~~~~~{.haskell}
let index = createListing "index.html"
                          ["templates/postitem.html"]
                          (take 3 postPages)
                          [("title", Left "Home")]
~~~~~

The result of this will be a `HakyllAction () Context`. This `Context`'s `$body`
will contain a concatenation of all the 3 posts, rendered with the
`templates/postitem.html` template.

Now, we only have to render it: first using the `index.html` template - which
adds some more information to our index - then using the
`templates/default.html` template.

~~~~~{.haskell}
renderChain ["index.html", "templates/default.html"] index
~~~~~

Note that the `index.html` in the `renderChain` list is also a template. Now,
you might want to take your time to read the `index.html` template and the other
files in the zip so you understand what is going on here.

## The gist of it

- You can find blogposts using `getRecursiveContents`.
- The convention is to call them `yyyy-mm-dd-rest-of-title.extension`. This
  allows us to sort them easily.
- You can use `createCustomPage` or `createListing` to create custom pages and
  simple listings.
