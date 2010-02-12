---
title: RSSBlog
what: adds an RSS feed to the blog from the previous tutorial
---

## Adding RSS to our simple blog

In this tutorial, we're going to add an RSS feed to the blog we wrote in
[the previous tutorial](tutorial2.html). Here is a
[zip file containing the source](examples/rssblog.zip).

An RSS feed looks like this:

~~~~~{.xml}
<?xml version="1.0" ?>
<rss version="2.0">
  <channel>
    <title>The SimpleBlog</title>
    <link>http://example.com/</link>
    <description>Simple blog in hakyll</description>
    <item>
      <title>Title goes here</title>
      <link>http://example.com/post.html</link>
      <description>
        A description is optional.
      </description>
    </item>
  </channel>
</rss>
~~~~~

Note that, obviously, there can be more than one item. We're going to use a
template to render this. This is where `templates/rss.xml` comes in:

~~~~~{.xml}
<?xml version="1.0" ?>
<rss version="2.0">
  <channel>
    <title>The SimpleBlog</title>
    <link>http://jaspervdj.be/</link>
    <description>Simple blog in hakyll</description>
    $body
  </channel> 
</rss>
~~~~~

We thus render our feed with the following code (no, I didn't define `rss`
yet - I'm going to work from bottom to top here, it's easier to explain it
that way).

~~~~~{.haskell}
renderChain ["templates/rss.xml"] rss
~~~~~

This, as you can see, is a regular render chain, once again. We need make a
`Renderable` that "fills in" the `$items` identifier. We're going to do this
using a [custom page](tutorial2.html#custom-pages).

## Custom pages again

Note that we do not have to include all posts in the rss feed - only a few
recent ones. We'll settle on the latest three here.

We want to render every post using the following template,
`templates/rssitem.xml`:

~~~~~{.haskell}
<item>
  <title>$title</title>
  <link>http://example.com/$url</link>
  <description>$title by $author</description>
</item>
~~~~~

Since we build on the previous example, we still have our `renderablePosts`
list. We'll be using it again to create a listing:

~~~~~{.haskell}
let rss = createListing "rss.xml" "templates/rssitem.xml"
                        (take 3 renderablePosts) []
~~~~~

Note that we give `rss.xml` as destination url, because that is where we want
our `CustomPage`. We give a template to render every post with
(`"templates/rssitem.xml"`) and no additional context (so we use the empty list
`[]`).

## Adding a link to the feed

According to the w3 organization,
[you should add \<link\> tags](http://www.w3.org/QA/Tips/use-links) to your
documents, so we'll do this. In `templates/default.html`:

~~~~~~{.html}
<head>
  <title>SimpleBlog - $title</title>
  <link rel="stylesheet" type="text/css" href="$$root/css/default.css" />
  <link rel="alternate"
        type="application/rss+xml"
        title="SimpleBlog"
        href="$root/rss.xml" />
</head>
~~~~~~

This makes most browsers see the rss feed, and show it in the address bar.
If you want, you can also add a pretty button to your blog linking to
`rss.xml`.

## That's it!

Yep, that's it. Feel free to play with the source code in
[the zip file](examples/rssblog.zip) and extend the blog further. As always,
all questions are welcome on the
[google discussion group](http://groups.google.com/group/hakyll).
