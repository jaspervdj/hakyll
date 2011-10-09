---
title: Arrow Magic: Metadata Dependent Page Generation
author: Florian Hars
---

## Supporting a "published: false" attribute on pages

Many content management systems or blog platforms support
some kind of workflow that display articles differently or
not at all depending on which state the article is in, for
example whether it has a "published" attribute or not.
Hakyll has no built-in support for anything like this, but since
its compilers are just arrows, it is easy to implement arbitrary
metadata dependent behaviour for rendering pages.

Let's start by adding support for a "published" attributes to the
`simpleblog` example. We want to consider a blog post published if it
has a `published` metadata element that does not have the value
`false`. A function to test for this is simple:

~~~~~{.haskell}
isPublished :: Page a -> Bool
isPublished p =
  let published = getField "published" p in
  published /= "" && published /= "false"       
~~~~~

The next step is to write a function that tags a page with its
published status, which can be either unpublished or published, using
the standard `Either` datatype and then transform this function
into a `Compiler`. The latter can be done with the standard `arr`
function from `Control.Arrow`, which lifts a function into an arrow:

~~~~~{.haskell}
isPagePublished :: Compiler (Page a) (Either (Page a) (Page a))
isPagePublished = arr (\p -> if isPublished p then Right p else Left p)
~~~~~

For the next processing steps we now need a compiler that takes an
`Either (Page a) (Page a)` instead of the usual `Page a` as an
input. But the former can be built up from the latter using some
standard combinators from the `Control.Arrow` library.  The simplest
one is `|||`, which takes two comilers (arrows) with the same output
type and returns a new compiler that takes an `Either` of the input
types of the Compilers as an input. Maybe we just want to render our
unpublished posts with a big warning that they are provisional, so we
just want to render the unpublished `Left` pages with another template
than the published `Right` pages:

~~~~~{.haskell}
  match "posts/*" $ do
    route   $ setExtension ".html"
    compile $ pageCompiler
        >>> isPagePublished
        >>> (applyTemplateCompiler "templates/embargo-post.html"
             ||| applyTemplateCompiler "templates/post.html")
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
~~~~~

With the conditional rendering in place, the next step is to hide
the unpublished posts from the homepage and the list of posts.
Both lists are generated from the results of a requireAllA call.
The last argument of requireAllA is a Compiler, and requireAllA
passes a pair consiting of the currently rendered page and a list
of all the required pages. All we have to do to suppress the pages
is to write a Compiler that takes such a pair as input, leaves the
first element of the pair unchanged and filters out all the unpublished
pages from list in the second element of the pair and then pass the
output from this compiler to the existing compilers handling the
list generation for the `index` and `posts` pages.

Again, we can use a function from `Control.Arrow` to build this
compiler from simpler ones, in this case it is `***`, which combines
two arrows to one arrow from pairs to pairs. For our purposes, we
combine the identity arrow, which leaves its input unchanged, and an
ordinary `filter` on a list lifted into the compiler arrow:

~~~~~{.haskell}
filterPublished :: Compiler (Page a, [Page b]) (Page a, [Page b])
filterPublished = id *** arr (filter isPublished)
~~~~~

All that remains to do is to chain this compiler in front of the existing
compilers passed to requireAllA in the code for `posts.html`

~~~~~{.haskell}
    >>> requireAllA "posts/*" (filterPublished >>> addPostList)
~~~~~

and for `index.html`:

~~~~~{.haskell}
    >>> requireAllA "posts/*"
          (filterPublished
           >>> (id *** arr (take 3 . reverse . sortByBaseName))
           >>> addPostList)
~~~~~

You may have noticed that the code for the index page uses the same
`id *** something` construct to extract some elements from the list
of all posts.

## Don't generate unpublished pages at all

The above code will treat unpublished posts differently and hide them
from all lists of posts, but they will still be generated, and someone
who knows their URLs will still be able to access them. That may be
what you need, but sometimes you might want to suppress them
completely. The simplest way to do so is to leave the rendering
pipeline for `"posts/*"` unchanged and just add the `isPagePublished`
compiler at the end. This will not compile, since hakyll knows how to
write a `Page String`, but not how to write an `Either (Page String)
(Page String)`. But that can be amended by a simple type class
declaration:

~~~~~{.haskell}
instance Writable b => Writable (Either a b) where
  write p (Right b) = write p b
  write _ _ = return ()
~~~~~

Now hakyll will happily generate published pages and ignore
unpublished ones. This solution is of course slightly wasteful, as at
will apply all the templates to an unpublished page before finally
discarding it. You can avoid this by using the `+++` function, which
does for the sum datatype `Either` what `***` does for the product
type pair:

~~~~~{.haskell}
  match "posts/*" $ do
    route   $ setExtension ".html"
    compile $ pageCompiler
       >>> isPublishedPage
       >>> (id +++ (applyTemplateCompiler "templates/post.html"
                    >>> applyTemplateCompiler "templates/default.html"
                    >>> relativizeUrlsCompiler))
~~~~~

The other problem with this solution is more severe: hakyll will no
longer generate the index and posts pages due to a rare problem in
haskell land: a runtime type error. Hakyll tries to be smart and reuse
the parsed pages from the `match "posts/*"` when processing the
`requireAllA "posts/*"` calls by caching them. But the compilers there
still expect a list of pages instead of a list of eithers, so we have
to replace `filterPublised` with something that works on the
latter. Luckily (or, probably, by design), `Data.Either` provides just
the function we need, so the new filtering compiler is actually
shorter that the original, even though it has a more intimidating
type:

~~~~~{.haskell}
filterPublishedE :: Compiler (Page a, [Either (Page b) (Page b)]) (Page a, [Page b])
filterPublishedE = id *** arr rights
~~~~~

## Timed releases

Exploiting the fact that compilers are arrows, we can do more mixing
and matching of compilers to further refine how hakyll deals with page
attributes like `published`. Maybe you want `cron` to update your blog
while you are on vacation, so you want posts to be considered
published if the `published` field is either `true` or a time in the
past.

If you happen to live in the UK in winter or enjoy to do time zone
calculation in your head, your new function to test if a page is
published and the compiler derived from it might then look like this

~~~~~{.haskell}
isPublishedYet :: Page a -> UTCTime -> Bool
isPublishedYet page time =
  let published = getField "published" page in
  published == "true" || after published
  where
  after published =
    let publishAt = parseTime defaultTimeLocale "%Y-%m-%d %H:%M" published in
    fromMaybe False (fmap (\embargo -> embargo < time) publishAt)

isPagePublishedYet :: Compiler (Page a, UTCTime) (Either (Page a) (Page a))
isPagePublishedYet = arr (\(p,t) -> if isPublishedYet p t then Right p else Left p)
~~~~~

This compiler has a pair of a page and a time as its input, and we can
use yet another function from `Control.Arrow` to construct a compiler
that generates the input for it, the function `&&&`. It takes two
compilers (arrows) with the same input type and constructs a compiler
from that type to a pair of the output types of the two compilers.
For the first argument we take the `pageCompiler` which we already
call at the beginning of the page compilation. The second argument
should be a compiler with the same input type as `pageCompiler` that
returns the current time. But the current time lives in the IO monad
and does not at all depend on the resource the current page is
generated from, so we have to cheat a little bit by calling
`unsafeCompiler` with a function that ignores its argument and returns
an `IO UTCTime`, which `unsafeCompiler` will unwrap for us:

~~~~~{.haskell}
  match "posts/*" $ do
    route   $ setExtension ".html"
    compile $ (pageCompiler &&& (unsafeCompiler (\_ -> getCurrentTime)))
      >>> isPagePublishedYet
      >>> (id +++ ( ... as above ...))
~~~~~

This is all we have to change if we don't generate unpublished pages
at all. If we just hide them from the lists, the call to `|||`
discards the information that a page is not (yet) published that was
encoded in the `Either`. In that case we could use the `setField`
function from `Hakyll.Web.Page.Metadata` to rewrite the `published`
field of the left and right pages in `isPagePublished(Yet)` to
canonical values that the original `isPublished` function called from
`filterPublished` understands:

~~~~~{.haskell}
isPagePublishedYet = arr (\(p,t) -> if isPublishedYet p t then pub p else unpub p)
  where
    pub p = Right $ setField "published" "true" p
    unpub p = Left $ setField "published" "false" p
~~~~~

The final version of this code can be found in the timedblog example,
together with the required `import` statements.