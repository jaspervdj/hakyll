---
title: Tutorial (Part II)
what: elaborates a little on writing pages and templates
---

## The structure of a Page

The most important thing to realize is that a `Page` is just a key-value
mapping. Another example:

    ---
    title: About
    author: Mia Wallace
    ---
    Hello there! This is
    a simple about page.

This will produce the following mapping:

- `title`: About
- `author`: Mia Wallace
- `body`: Hello there! This is a simple about page.

`body` is the traditional name for the main body part of a page. If the page has
a `.markdown` extension for example, this would also be rendered by pandoc. But
pages are more flexible. The following is also a valid page:

    Hello there! This is
    a simple about page.

This will produce one key-value pair:

- `body`: Hello there! This is a simple about page.

But the `Page` parser can do more than this. You can add extra sections, apart
from the body, and even leave out the body.

    ---
    author: Vincent Vega
   
    --- prelude
    A small introduction goes here. I can write *markdown*
    here, by the way. Well, assuming this page has a
    `.markdown` extension.
   
    --- main
    I can write some more things here.

This will produce the following:

- `author`: Vincent Vega
- `prelude`: A small introduction goes here. I can write *markdown* here, by the
  way. Well, assuming this page has a `.markdown` extension.
- `main`: I can write some more things here.

The example from this tutorial (we will see later) uses this to build a
three-column system for the website, separating content from layout.

## Combining pages

Now you know that `Page`s, and `Renderable`s in general, are basically nothing
more than key-values mappings, it is time to abuse this fact. There is another
`Renderable` type we haven't talked about before: a `CombinedRenderable`.

The type signature of the `combine` function does a pretty good job at
explaining it:

~~~~~{.haskell}
combine :: (Renderable a, Renderable b)
        => a -> b -> CombinedRenderable a b
~~~~~

This means we can take two `Renderable` values and combine them. This is
basically a `Map.union`: The result will contain all keys from `a`, and all
keys from `b`. If a key is present in both `Renderable`s, the value from `a`
will be chosen. This is, for example, always the case with an `url` (since
all `Renderable` types always have an url).

Combining two `Renderable`s, but setting a different `url` is quite common, so
there is another function that helps us here:

~~~~~{.haskell}
combineWithURL :: (Renderable a, Renderable b)
               => FilePath -> a -> b -> CombinedRenderable a b
~~~~~

## The example

Now that we have the tools, we'll get on to the example. This time, we'll
be making a more advanced brochure site. Here
[is a zip file](examples/morepages.zip) containing the source code for the
tutorial.

Every page consists of three sections, originally named `section1`, `section2`
and `section3`. So our pages look more or less like this:

    ---
    title: About

    --- section1
    ## Mattis
    Nullam imperdiet sodales orci vitae molestie. Nunc...

    --- section2
    ## Orci
    Vivamus eget mauris sit amet nulla laoreet lobortis.
    Nulla in...

    --- section3
    ## Augue
    In urna ante, pulvinar et imperdiet nec, fermentum ac...

The cool thing is we do not have to specify how these will be layed out. In our
template, we decide to use a simple three column system:

~~~~~{.html}
<div class="column"> $section1 </div>
<div class="column"> $section2 </div>
<div class="column"> $section3 </div>
~~~~~

The columns are then floated using css. So far so good, but what if we wanted
an additional text block on every page? An easy solution would be to add this
to the template, but then our layout-content separation idea will be broken
again. So we simply add to the template:

~~~~~{.html}
<div class="footer"> $footer </div>
~~~~~

And now we will use `combine` to put the footer on every page. We write a small
auxiliary function that combines a given `Renderable` with the footer:

~~~~~{.haskell}
withFooter a = a `combine` createPagePath "footer.markdown"
~~~~~

Now, were we previously wrote:

~~~~~{.haskell}
render "about.markdown"
where render = renderChain ["templates/default.html"]
             . createPagePath
~~~~~

We simply have to add our footer:

~~~~~{.haskell}
render "about.markdown"
where render = renderChain ["templates/default.html"]
             . withFooter
             . createPagePath
~~~~~

And now every page will include the footer.

## That's all folks

I hope this tutorial was clear enough to teach you the concepts of pages and
combining renderables. As always, questions and feedback are welcome at the
[google discussion group](http://groups.google.com/group/hakyll).
