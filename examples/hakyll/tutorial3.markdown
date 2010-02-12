---
title: SimpleBlog
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

    title: A first post
    author: Julius Caesar
    date: November 5, 2009
    ---
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
    Vivamus pretium leo adipiscing lectus iaculis lobortis.
    Vivamus scelerisque velit dignissim metus...

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

Currently, there are 4 renderable datatypes in Hakyll:

- `Page`: The result of any rendering action. It is generally not recommended
  to use pages a lot, because they cannot check dependencies (and therefore,
  you would always regenerate your entire site if you use pages the wrong way).
- `PagePath`: Basically just a `FilePath` in a box. Internally, this will use
  a `Page` for rendering, but `PagePath` provides better dependency checking
  and works on a higher level.
- `CustomPage`: Basically the name says it - the preferred way of creating
  custom pages in Hakyll.
- `CombinedRenderable`: A way of combining two other `Renderable`s - this is
  explained in tutorial 2.

We will use a `CustomPage` here. Basically, all `Renderable` datatypes are in
the end just `key: value` mappings. A CustomPage is created using the
`createCustomPage` function, which has the following type signature:

~~~~~{.haskell}
createCustomPage :: FilePath
                 -> [FilePath]
                 -> [(String, Either String (Hakyll String)]
~~~~~

The first argument is the `url` of the page to generate. For our index page,
this will be, `index.html`. The second argument is _a list of dependencies_.
Basically, you should here give a list of files on which your custom page
depends.

The last argument is obviously our `key: value` mapping. But why the `Either`?
This, once again, is about dependency handling. The idea is that you can choose
which type to use for the value:

- `String`: Simply a `String`.
- `Hakyll String`: Here, you can give an arbitrary `Hakyll` action that will
  result in a String. However - this action _will not be executed_ when the file
  in `_site` is up-to-date.

However, in this specific case - a list of posts - there is an easier, and more
high-level approach[^1]. Let's look at the signature of `createListing`:

~~~~~{.haskell}
createListing :: (Renderable a)
              -> String
              -> FilePath
              -> [a]
              -> [(String, String)]
~~~~~

[^1]: Since Hakyll-1.3 onwards.

The first argument is the destination url. For our blog, this is of course
`index.html`. The second argument is a template to render _each renderable_
with. We use `templates/postitem.html` here. This is, as you can see, a simple
template:

~~~~~{.html}
<li>
    <a href="$$root/$url">$title</a>
    - <em>$date</em> - by <em>$author</em>
</li>
~~~~~

We then give a list of renderables. For our index, these are the 3 last posts.
The last argument of the `createListing` functions lets you specify additional
key-value pairs. We use this to set the title of our page. So, we create our
index page using:

~~~~~{.haskell}
let index = createListing "index.html"
                          "templates/postitem.html"
                          (take 3 renderablePosts)
                          [("title", "Home")]
~~~~~

The result of this will be a `CustomPage`. The body of this page will contain
a concatenation of all the renderables, rendered with the
`templates/postitem.html` template.

Now, we only have to render it: first using the `index.html` template, then
using the `templates/default.html` template.

~~~~~{.haskell}
renderChain ["index.html", "templates/default.html"] index
~~~~~

Note that the `index.html` in the `renderChain` list is also a template. Now,
take your time to read the `index.html` template and the other files in the zip
so you understand what is going on here.

## That's that

If you have any more questions, feel free to ask them on the
[google discussion group](http://groups.google.com/group/hakyll).

There is a [next tutorial](tutorial4.html), explaining how to add an RSS feed
to our sample blog.
