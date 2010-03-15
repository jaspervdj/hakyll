---
title: Creating feeds
what: adds an rss feed to the simple blog
---

## Adding Feeds

In this tutorial, we're going to add an RSS feed to the blog we wrote in the
previous part. Here is a [zip file] containing the source.

[zip file]: examples/feedblog.zip

You will be glad to hear that Hakyll has native support for RSS as well as Atom
feeds[^1]. This simplifies our object a lot.

[^1]: Since Hakyll-2.0

This is the first time that the absolute URL of your site you have to give to
the `hakyll` function actually matters. That's because the specifications of
those feed formats dictate you need the full URL of them.

## Creating a configuration

The first thing to do is creating a configuration for your feed. You could
place this code outside of your main function, as is done in the example.

~~~~~{.haskell}
myFeedConfiguration = FeedConfiguration
    { feedUrl         = "rss.xml"
    , feedTitle       = "SimpleBlog RSS feed."
    , feedDescription = "Simple demo of a feed created with Hakyll."
    , feedAuthorName  = "Jasper Van der Jeugt"
    }
~~~~~

Note that we enter the url of the feed in our configuration. So the function
to render our feed only takes two arguments, the configuration and a list of
items to put in the feed. Let's put the three most recent posts in our feed.

~~~~~{.haskell}
renderRss myFeedConfiguration (take 3 postPages)
~~~~~

## But it's not that easy

If you look at our generated RSS feed (build the site), you will see
`$description` tags appearing in our final render. We don't want that! How
did they get there in the first place?

To render feeds, Hakyll expects a number of fields in the renderables you put
in the feed. They are:

- `$title`: Title of the item. This is set in our posts, since we use a `title`
  metadata field.
- `$url`: Url of the item. This is automatically set by Hakyll, so you shouldn't
  worry about it.
- `$description`: A description of our item to appear in the feed reader.

The latter is obviously the problem: we don't have a description in our posts.
In fact, we would like to copy the `$body` key to the `$description` key, so
people can read the full post in their feed readers.

## Where arrows come in

The `Text.Hakyll.ContextManipulations` module contains a number of simple
functions that create Arrows for us. One of these functions is `copyValue`,
which takes a source and a destination key. So, we need to pass our
items through this Arrow first.

~~~~~{.haskell}
renderRss myFeedConfiguration $
    map (>>> copyValue "body" "description") (take 3 postPages)
~~~~~

And that's that, now our feed gets rendered properly. Exercise for the reader
is to add a Atom feed[^2].

[^2]: Hint: look around in the [reference]($root/reference.html).

## The gist of it

- Hakyll has native support for RSS and Atom feeds.
- The items must contain `$title` and `$description` fields.
- Arrows can be used to copy values in a `Context`.
