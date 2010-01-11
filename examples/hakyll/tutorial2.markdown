---
title: Tutorial (Part II)
what: creates a simple blog
---

## Creating a simple blog with Hakyll

After we created a simple brochure site, we're going to try something more
advanced: we are going to create a simple blog system.

A [zip file containing the source](examples/simpleblog.zip) for this
tutorial is also available.

Blogs, as you probably know, are composed of posts. In Hakyll, we're going
to use simple pages for posts. All posts are located in the `posts`
directory. But we're not going to use the `directory` command here - you will
see why later. First, some trivial things like css.

~~~~~{.haskell}
main = hakyll $ do
    directory css "css"
~~~~~

## Finding the posts, and a bit about renderables

`Text.Hakyll.File` contains a handy function `getRecursiveContents`, which will
provide us with all the blog posts. The blog posts have a
`yyyy-mm-dd-title.extension` naming scheme. This is just a simple trick so we
can sort them easily, you could of course name them whatever you want. They
contain some metadata, too:

~~~~~{.markdown}
---
title: A first post
author: Julius Caesar
date: November 5, 2009
---
Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
Vivamus pretium leo adipiscing lectus iaculis lobortis.
Vivamus scelerisque velit dignissim metus...
~~~~~

Now, we find the posts and sort them reversed:

~~~~~{.haskell}
-- Find all post paths.
postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
~~~~~

Our `postPaths` value is now of the type `[FilePath]`. `FilePath` is no
instance of `Renderable`, but `PagePath` is:

~~~~~{.haskell}
let renderablePosts = map createPagePath postPaths
~~~~~

We have two templates we want to render our posts with: first we would like to
render them using `templates/post.html`, and we want to render the result
using `templates/default.html`. This can be done with the `renderChain`
function:

~~~~~{.haskell}
mapM_ (renderChain [ "templates/post.html"
                   , "templates/default.html"
                   ]) renderablePosts
~~~~~

Remember that the `renderChain` works by rendering the datatype using the first
template, creating a new page with the render result in the `body` field, and so
on until it has been rendered with all templates.

Now, we have the posts rendered. What is left is to generate some kind of index
page with links to those posts. We want one general list showing all posts, and
we want to show a few recent posts on the index page.

## Custom Pages

Currently, there are 3 renderable datatypes in Hakyll:

- `Page`: The result of any rendering action. It is generally not recommended
  to use pages a lot, because they cannot check dependencies (and therefore,
  you would always regenerate your entire site if you use pages the wrong way).
- `PagePath`: Basically just a `FilePath` in a box. Internally, this will use
  a `Page` for rendering, but `PagePath` provides better dependency checking
  and works on a higher level.
- `CustomPage`: Basically the name says it - the preferred way of creating
  custom pages in Hakyll.

We will use a `CustomPage` here. Basically, all `Renderable` datatypes are in
the end just `key: value` mappings. A CustomPage is created using the
`createCustomPage` function, which has the following type signature:

~~~~~{.haskell}
createCustomPage :: FilePath
                 -> [FilePath]
                 -> [(String, Either String (IO String)]
~~~~~

The first argument is the `url` of the page to generate. For our index page,
this will be, `index.html`. The second argument is _a list of dependencies_.
Basically, you should here give a list of files on which your custom page
depends.

The last argument is obviously our `key: value` mapping. But why the `Either`?
This, once again, is about dependency handling. The idea is that you can choose
which type to use for the value:

- `String`: Simply a `String`.
- `IO String`: Here, you can give an arbitrary `IO` action that will result
  in a String. However - this action _will not be executed_ when the file
  in `_site` is up-to-date.

First, let us define this `IO String` for our index page. We want to render
every post using a simple template:

~~~~~{.html}
<li>
    <a href="$root/$url">$title</a>
    - <em>$date</em> - by <em>$author</em>
</li>
~~~~~

When every post is rendered with this template, we then want to concatenate the
result. Since rendering and concatenating is pretty common, Hakyll provides us
with a high-level function to do this.

~~~~~{.haskell}
let recentPosts = renderAndConcat "templates/postitem.html"
                    (take 3 renderablePosts)
~~~~~

Now, creating our custom page is fairly straight-forward:

~~~~~{.haskell}
createCustomPage "index.html"
                 ("templates/postitem.html" : take 3 postPaths)
                 [ ("title", Left "All posts")
                 , ("posts", Right recentPosts)
                 ]
~~~~~

You can see our three arguments here. We're rendering `index.html`, then we tell
Hakyll on what files it depends - here the `templates/postitem.html` template
and the latest 3 posts. Finally, we give a `title` value to substitute in the
template, and the result of our concatenation. Of course, we also need to render
this custom page:

~~~~~{.haskell}
renderChain ["index.html", "templates/default.html"] $
    createCustomPage "index.html"
         ("templates/postitem.html" : take 3 postPaths)
         [ ("title", Left "All posts")
         , ("posts", Right recentPosts)
         ]
~~~~~

Note that the `index.html` in the `renderChain` list is also a template.

## That's that

If you have any more questions, feel free to ask them on the
[google discussion group](http://groups.google.com/group/hakyll).

There is a [next tutorial](tutorial3.html), explaining how to add an RSS feed
to our sample blog.
