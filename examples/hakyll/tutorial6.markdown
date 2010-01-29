---
title: Tutorial (Part VI)
what: explains how to use categories instead of tags
---

## Categories

Most people familiar with "tags" will also know the concept "categories".

![Tags illustration]($root/images/tutorial6-tags.png)

In fact, tags are harder to implement because they have to be represented as a
many-to-many relation, and categories are a simple 1-to-many relation.

![Tags illustration]($root/images/tutorial6-categories.png)

This is also the reason you can "simulate" categories using tags. In this
tutorial we will adapt the blog to use categories instead of tags. Here is
[a zip file](examples/categoryblog.zip) containing the files used in this
tutorial.

## Reading Categories

Tags are located in the `tags` metadata field. Since one post can only belong
in one category, a different approach was chosen here. The category of a post
is determined by the subfolder it is in. Here you see the directory layout for
our posts using categories:

    posts
    |-- coding
    |   |-- 2009-11-05-a-first-post.markdown
    |   |-- 2009-11-28-a-third-post.markdown
    |   `-- 2009-12-04-this-blog-aint-dead.markdown
    `-- random
        |-- 2009-11-10-another-post.markdown
        `-- 2009-12-23-almost-christmas.markdown

Because we find all our posts in different subdirectories, sorting them is a
little harder: we still want them sorted by date, so it boils down to sorting
them by "base name". I hope it does not surprise you Hakyll provides a function
for that:

~~~~~{.haskell}
postPaths <- liftM (reverse . sortByBaseName)
                   (getRecursiveContents "posts")
~~~~~

Now, we can use the `readCategoryMap` function instead of `readTagMap`, which
has the same signature, but assigns categories based on the folders the posts
are in.

~~~~~{.haskell}
categoryMap <- readCategoryMap "categoryMap" renderablePosts
~~~~~

The rest of the `hakyll.hs` is very similar to the one in the previous
tutorial, except we want to render a category list instead of a tag cloud.

## Rendering a category list

Because rendering a category list is quite easy, and it would be hard to
write a "general" function for this, hakyll does not provide such a function -
but it is not hard to write. First, we write an auxiliary function that produces
a list item for one category:

~~~~~{.haskell}
categoryListItem category posts =
    "<li>" ++ link category (categoryToURL category)
    ++ " - " ++ show (length posts) ++ " items.</li>"
~~~~~

This is nothing more that some string concatenation. The function that applies
this on every element in the `TagMap` is more interesting:

~~~~~{.haskell}
categoryList :: TagMap -> String
categoryList = uncurry categoryListItem <=< toList
~~~~~

This function might seem a little harder to understand, and figuring out how it
works is left as an exercise for the reader[^1].

[^1]: Hint: `>>=` is `concatMap` in the list monad.

We then add this to our index page, and we are done. Feel free to hack around
with the source code. If you still have questions, feel free to ask them at the
[google discussion group](http://groups.google.com/group/hakyll).
