---
title: How to write pages
what: elaborates a little on writing pages and templates
---

## The structure of a Page

The most important thing to realize is that a page is reduced to a `Context`,
and therefore is just a key-value mapping. Another example:

    ---
    title: About
    author: Mia Wallace
    ---
    Hello there! This is
    a simple about page.

This will produce the following mapping:

- `$title`: About
- `$author`: Mia Wallace
- `$body`: Hello there! This is a simple about page.

`$body` is the traditional name for the main body part of a page. If the page
has a `.markdown` extension for example, this would also be rendered by pandoc.
But pages are more flexible. The following is also a valid page:

    Hello there! This is
    a simple about page.

This will produce one key-value pair:

- `$body`: Hello there! This is a simple about page.

But Hakyll can do more than this. You can add extra sections, apart from the
body, and even leave out the body.

    ---
    author: Vincent Vega
   
    --- prelude
    A small introduction goes here. I can write *markdown*
    here, by the way. Well, assuming this page has a
    `.markdown` extension.
   
    --- main
    I can write some more things here.

    ---
    The body comes last, and is optional.

This will produce the following:

- `$author`: Vincent Vega
- `$prelude`: A small introduction goes here. I can write *markdown* here, by the
  way. Well, assuming this page has a `.markdown` extension.
- `$main`: I can write some more things here.
- `$body`: The body comes last, and is optional.

The example from this tutorial (we will see later) uses this to build a
three-column system for the website, separating content from layout.

## Combining Contexts

Now you know that pages, and `Context`s in general, are basically nothing more
than key-values mappings, it is time to abuse this fact. There is another
way to create a `Context`, called `combine`.

The type signature of the `combine` function does a pretty good job at
explaining it:

~~~~~{.haskell}
combine :: HakyllAction () Context
        -> HakyllAction () Context
        -> HakyllAction () Context
~~~~~

This means we can take two `Context`s values and combine them. This is
basically a `Map.union`: The result will contain all keys from both `Context`s,
with there corresponding values. If a key is present in both `Context`s, the
value from the first argument will be chosen. This is, for example, almost
always the case with the `$url` field (since almost all `Context`s have an url
in Hakyll).

Combining two `Context`s, but overriding the `$url` is quite common, so there is
another function that helps us here:

~~~~~{.haskell}
combineWithUrl :: FilePath
               -> HakyllAction () Context
               -> HakyllAction () Context
               -> HakyllAction () Context
~~~~~

## The example

Now that we have the tools, we'll get on to the example. This time, we'll
be making a more advanced brochure site. Here [is a zip file] containing the
source code for the tutorial.

[is a zip file]: $root/examples/morepages.zip

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

And now we will use `combine` to put the footer on every page - so we need to
add the footer page to every `Context`. We write a small auxiliary function
that combines a given `Context` with the footer:

~~~~~{.haskell}
withFooter = flip combine $ createPage "footer.markdown"
~~~~~

Note that we use `flip` here - we want `footer.markdown` to be our second
argument. That is because Hakyll will take the `$url` from the first `Context`,
so all pages would be rendered to `footer.html` - obviously not what we want.
Now, were we previously wrote:

~~~~~{.haskell}
render "about.markdown"
where render = renderChain ["templates/default.html"]
             . createPage
~~~~~

We simply have to add our footer:

~~~~~{.haskell}
render "about.markdown"
where render = renderChain ["templates/default.html"]
             . withFooter
             . createPage
~~~~~

And now every page will include the footer.

## The gist of it

- Pages are just key-value mappings.
- You can have multiple sections in every page.
- Combine pages using the `combine` function.
