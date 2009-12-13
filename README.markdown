# Hayll

Hakyll is a simple static site generator library in Haskell. It is mostly
inspired by [Jekyll](http://github.com/mojombo/jekyll), but I like to
believe it is simpler. An example site where it is used is
[my personal homepage](http://jaspervdj.be) of which
[the source code](http://jaspervdj.be/snapshot.tar.gz) is available as a
reference.

## Installation

    cabal install hakyll

## Configuration

Inspired by [xmonad](http://xmonad.org), a small Haskell program is used as
configuration file. In this file, you give instructions on how the site should
be generated. In the rest of this document, we will examine a small example.

This is our directory layout:

    |-- _cache
    |-- _site
    |-- favicon.ico
    |-- hakyll.hs
    |-- images
    |   `-- foo.png
    |-- templates
    |   |-- default.html
    |   `-- sample.html
    `-- text.markdown

The `_cache` and `_site` directories will be automatically created by hakyll.

## Static files

Static files can be rendered using the `static` function. This function
ensures the files will copied when you compile the site. Hakyll is smart enough
to know when files have changed, and will check the modification time of a file
before copying.

For convenience reasons, there is also a `staticDirectory` function, which works
recursively.

    main = do
        static "favicon.ico"
        staticDirectory "images"
        staticDirectory "css"

## Pages

Pages can be written in html, markdown, LaTeX, and basically anything pandoc
supports. They can also contain metadata, which are always key-value mappings.

    ---
    author: Jasper Van der Jeugt
    title: A sample markdown post
    ---
    # A sample markdown post

    This is a sample markdown post. It supports pandoc extensions
    like code highlighting. For example:

    ~~~~{.haskell}
    main = putStrLn "Hello World!"
    ~~~~

Metadata is always placed in the header of a file, and is delimited by a `---`
string. The metadata can only contain simple key-value pairs. We can now read
in this page using the `Text.Hakyll.Page.readPage` function. This will return a
`Page`, which is actually just a `Map String ByteString`. In this example, the
map would consist of the following key-value pairs:

- `author`: `Jasper Van der Jeugt`
- `title`: `A sample markdown post`
- `body`: The rest of the file (rendered to html).
- `url`: `text.html` (the original filename was `text.markdown`, the extension
  was changed to html).

## Templates

In hakyll, there is a strict separation between pages and templates. Templates,
for example, cannot contain metadata.

    <h2> $title </h2>
    by <strong> $author </strong>

    $body

Templates are rendered using the Haskell `Text.Template` library. This means
that in your template, you can use `$identifier`, and it will be replaced by
the value of `identifier`.

With this template we could, for example, render the file we saw in the
previous section. It would go like this:

    page <- readPage "text.markdown"
    rendered <- render "templates/sample.html" page
    writePage rendered

This reads in `text.markdown`, renders it and writes it to the site destination
(`_site/text.html`). The result of a `render` action is an `IO Page`, the
metadata will be copied from the original page, and the body will be replaced by
the rendering result. This means we can combine rendering actions. Given another
template `templates/default.html`:
    <html>
        <head>
            <title> $title </title>
        </head>
        <body>
            $body
        </body>
    </html>

We can now combine the rendering actions (I use `>>=` notation here):
    readPage "text.markdown" >>=
        render "templates/sample.html" >>=
        render "templates/default.html" >>=
        writePage

Jolly good fun and all that, but you can imagine that when we render over nine
thousand posts, our generator will be busy for a while. That's why we have the
`depends` function. It takes a url and a list of dependencies as arguments, and
an IO action. The trick is that this IO action will only be executed if any of
the dependencies is newer than the url given. In our example, we would write:

    depends "text.html" ["text.markdown", "templates/sample.html", "templates/default.html"]
            (readPage "text.markdown" >>= render "templates/sample.html" >>=
             render "templates/default.html" >>= writePage)

Not exactly the prettiest code I've ever seen. Because rendering a page with a
number of templates is very common, there's a `renderChain` function to do this
for us. The above can be replaced by

      renderChain ["templates/sample.html", "templates/default.html"] $
            createPagePath "text.markdown"

The `renderChain` function will automatically check dependencies and write the
page. In fact, it is recommended that you __always__ use this, even when there's
only one template in the chain.

## More advanced things

Sometimes, you want to create a `Page` from scratch, without reading from a
file. There are functions to do that for you, and I suggest you read the
documentation of `Text.Hakyll.Page`. As a more advanced example, I will explain
the RSS system I wrote for my website.

    |-- generate.hs
    |-- posts
    |   `-- 2009-12-02-a-first-post.markdown
    `-- templates
        |-- rss.xml
        `-- rssitem.xml

Our post contains some metadata:</p

    ---
    title: A first post
    date: December 2, 2009
    about: A post describing the why and the how of the technical setup of this blog.
    ---

    # A first post

    A first post describing the technical setup of this blog, for that is

The `templates/rssitem.xml` file is a template for rendering one post to an rss
item:

    <item>
        <title> $title </title>
        <link>http://jaspervdj.be/$url</link>
        <description> $about </description>
    </item>

Now a template for rendering the whole rss feed, `templates/rss.xml`:

    <?xml version="1.0"?>
    <rss version="2.0">
        <channel>
            <title>jaspervdj - a personal blog</title>
            <link>http://jaspervdj.be/</link>
            <description>Personal blog of jaspervdj</description>
            $items
        </channel> 
    </rss>

Alright, let's get coding. We first want a list of all posts, sorted so the most
recent entries are first in the list.

    postPaths <- liftM (L.reverse . L.sort) $ getRecursiveContents "posts"

Note that this sorting works because the posts have a
`yyyy-mm-dd-title.extension` naming scheme. We want the paths as `PagePath` and
not as `FilePath` or `String`, because `PagePath` is an instance of
`Renderable`. Also, we only want the 5 most recent posts.

    let renderablePosts = map createPagePath postPaths
    let recentItems = renderAndConcat "templates/postitem.html" $ take 5 renderablePosts

The `renderAndConcat` function takes a template and a list of `Renderable`
items. It renders all renderables with the given template, and concatenates the
result. Note that the concating and reading of pages is not executed yet,
because of laziness. This helps us, since we want to use the modification
timestamps of files, and not render everything every time.

let rssPage = createCustomPage "rss.xml"
        ("templates/rssitem.xml" : postPaths) [("items", Right recentItems)]

We now created the custom rss page. The `createCustomPage` is a function that
produces a `CustomPage`, which is an instance of `Renderable`. `"rss.xml"` is
our destination url. We then give a list of extra dependencies that were used
to generate the custom page, so Hakyll can check modification stamps. The last
argument is the key-value mapping of our `CustomPage`. Note that the type for
values is `Either String (IO ByteString)`. So, we can either give a simple
string, or an IO action that results in a `ByteString`. The benefit of this is
that the IO action will not be executed if the page in `_site/` is already
up-to-date. Now, we only need to render it using our rss template.

    renderChain ["templates/rss.xml"] rssPage

That's it. Now we have a rss feed that is generated only when it is not already
up-to-date.
