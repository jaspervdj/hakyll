# Hakyll

Hakyll is a simple static site generator in Haskell. It is mostly inspired by
[Jekyll](http://github.com/mojombo/jekyll), but I like to believe it is
simpler.

## Configuration

Inspired by [xmonad](http://xmonad.org), a small Haskell program is used as
configuration file. In this file, you give instructions on how the site should
be generated. In the rest of this document, we will examine a small example.

This is our directory layout:

    |-- _site
    |-- favicon.ico
    |-- hakyll.hs
    |-- images
    |   `-- foo.png
    |-- templates
    |   |-- default.html
    |   `-- sample.html
    `-- text.markdown

## Static files

Static files can be rendered using the `static` command. This command ensures
the files will copied when you compile the site.

For convenience reasons, there is also a `staticDirectory` command, which works
recursively.

    main = do
        static "favicon.ico"
        staticDirectory "images"
        staticDirectory "css"

## Pages

Pages can be written in html or markdown (altough it would be a trivial task
to add anything pandoc supports, just ask me if you want anything to be added).
They can also contain metadata, which are always key-value mappings.

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

Metadata is always placed in the beginning of a file, and is delimited by a
`---` string. The metadata can only contain simple key-value pairs. We can
now read in this page using the `Text.Hakyll.Page.readPage` function. This
will return a `Page`, which is actually just a `Map String String`. In this
example, the map would consist of the following key-value pairs:

- `author`: `Jasper Van der Jeugt`
- `title`: `A sample markdown post`
- `body`: The rest of the file (rendered to html).
- `url`: `text.html` (the original filename was `text.markdown`, the extension
  was changed to html).

## Templates

In hakyll, there is a strict separation between pages and templates. Templates,
for example, cannot contain metadata.

    <h2> $title </h2>
    by <strong> $author </title>

    $body

Templates are rendered using the Haskell `Text.Template` library. This means
that in your template, you can use `$identifier`, and it will be replaced by
the value of `identifier`.

With this template we could, for example, render the file we saw in the previous
section. It would go like this:

    page <- readPage "text.markdown"
    render <- renderPage "templates/sample.html" page

Now, `render` will be a `Page` containing all metadata from `page`, but the
`body` key would be replaced by the substitution. This means we can combine
rendering actions. Given another template `templates/default.html`:

    <html>
        <head>
            <title> $title </title>
        </head>
        <body>
            $body
        </body>
    </html>

We can now combine the rendering actions:

    page <- readPage "text.markdown"
    render <- (renderPage "templates/sample.html" page >>=
                renderPage "templates/default.html")

Of course, you can't really do anything with the render if you don't write it
to a file somewhere. That's why the function `renderAndWrite` exists:

    readPage "text.markdown" >>=
        renderPage "templates/sample.html" page >>=
        renderAndWrite "templates/default.html"

Now, where will this file be written? In `_site/text.html`, of course! That's
because the page still contains a key called `url`, which the renderAndWrite
function uses to determine the file destination.

## More advanced things

Sometimes, you want to create a `Page` from scratch, without reading from a
file. There are functions to do that for you, and I suggest you read the
documentation of `Text.Hakyll.Page`. As a more advanced example, I will
explain the RSS system I wrote for my website.

    |-- generate.hs
    |-- posts
    |   `-- 2009-12-02-a-first-post.markdown
    `-- templates
        |-- rss.xml
        `-- rssitem.xml

Our post contains some metadata:

    ---
    title: A first post
    date: December 2, 2009
    ---

    # A first post

    A first post describing the technical setup of this blog, for that is

The `templates/rssitem.xml` file is a template for rendering one post to an
rss item:

    <item>
        <title>$title</title>
        <link>http://jaspervdj.be/$url</link>
        <description>New blogpost: $title</description>
    </item>

Now a template for rendering the whole rss feed, `templates/rss.xml`:

    <?xml version="1.0" ?>
    <rss version="2.0">
        <channel>
            <title>jaspervdj - a personal blog</title>
            <link>http://jaspervdj.be/</link>
            <description>Personal blog of jaspervdj</description>
            $items
        </channel> 
    </rss>

Alright, let's get coding.

    -- Find all posts paths.
    postPaths <- liftM (L.reverse . L.sort) $ getRecursiveContents "posts"
    -- Read and render all posts with the rssitem.xml template
    -- Also, only render 5 posts.
    pages <- mapM readPage (take 5 postPaths)
    items <- mapM (renderPage "templates/rssitem.xml") pages
    -- Render the result
    renderAndWrite "templates/rss.xml" $ pageFromList [ ("items", concatPages items),
                                                        ("url", "rss.xml") ]

That's that. Now we have a nice rss feed.
