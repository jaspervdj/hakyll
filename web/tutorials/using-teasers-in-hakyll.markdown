---
title: Using teasers in Hakyll
author: Ivan Veselov
---

## Introduction

Sometimes it is convenient to have an excerpt of a post displayed on
the index page along with a "Read more..." link to encourage your
readers to read the rest of the post.

You can do this in Hakyll by using "teasers" functionality.

## Marking teasers in posts

First, you have to put a tag in your post which separates the teaser
from the rest of the article. We use `<!--more-->` for this to mimic
the [WordPress convention](http://codex.wordpress.org/Customizing_the_Read_More):

``` markdown
---
title: My teasering post
---
In this post I'll try to explain how to use teasers.

<!--more-->

And here I am proceeding with the explanation.

```

This is an HTML comment, so it doesn't have any visual impact on the
post, it is there solely for semantic purposes.

## Referring to teasers in templates

Now, we want to refer to the teaser in the template. We can do this
pretty intuitively by using `$teaser$` key:

``` html
<li class="post_item">
    $date$ - $title$
    <p>Teaser: $teaser$</p>
    <a href="$url$">Read more</a>
</li>
```

## Gluing together

The only thing left is to glue those things together. An important
question here is on what stage of the compilation we want to extract a
teaser. Usually, we want to use pandoc output, i.e. raw HTML without
any templates applied (since we do not want some surrounding
JavaScript or common text from the templates to be included in the
teaser). We can specify this by using snapshots: we save the snapshot
during compilation and load it to resolve `$teaser$` key later:

``` haskell
match "posts/*" $ do
  route $ setExtension ".html"
  compile $
     pandocCompiler
     -- save immediately after pandoc, but before the templates are applied
     >>= saveSnapshot "content"
     >>= loadAndApplyTemplate "templates/post.html" defaultContext
     ...
```

You can read more about snapshots in
[Snapshots tutorial](/tutorials/05-snapshots-feeds.html).

Then we use this snapshot while generating teasers using the
`teaserField` function:

``` haskell
    loadAndApplyTemplate
         "template/postitem.html"
         (teaserField "teaser" "content" <> defaultContext)
         item
```

Here, we've just added a new context which knows how to handle
`$teaser$` key to the default context (note that we passed the same
snapshot name `"content"` which we used while saving).

## Optional teasers

In case you don't add a `<!--more-->` comment, `$teaser$` will not be defined.
This means you can use something like:

```html
$if(teaser)$
    $teaser$
$else$
    $body$
$endif$
```

## Known issues

Since we use an HTML comment `<!--more-->` to separate the teaser,
sometimes `pandoc` can "eat" the comment while converting (for example
this happens with literate Haskell source). In order to overcome this
problem you may try to use something like this as a separator:

``` html
<div></div><!--more-->
```
