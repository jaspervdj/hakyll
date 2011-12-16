---
title: A Guide to the Hakyll Module Zoo
author: Brent Yorgey
---

The hakyll package [contains a bewildering array](/reference/) of
modules, and it's hard to know where to look when you are just getting
started---especially since many of them are only used by hakyll
internally and not that useful to website authors.  This guide
provides a quick reference to the contents of the most important and
useful modules.

## Core modules

These are the modules containing the fundamental tools and concepts
you need to get started building a site with hakyll.

* [Hakyll.Core.Compiler](/reference/Hakyll-Core-Compiler.html)

    This is one of the modules you should look at first.  It defines the
    fundamental `Compiler` type and has a bunch of documentation explaining how to
    use `Compiler`s and their `Arrow`-based interface.

    It also defines many primitive `Compiler`s as well as several
    variants on `require`, which allow you to bring together multiple
    resources to create a single output.

* [Hakyll.Core.Routes](/reference/Hakyll-Core-Routes.html)

    Specify where compiled items should go in the output site.

* [Hakyll.Core.Rules](/reference/Hakyll-Core-Rules.html)

    Specify which compilers and routes should be used on which
    resources.

    Also has combinators for grouping (necessary if you want to use
    the same resources for multiple purposes), creating outputs that
    are not based on any resources, and even dynamically generating
    compilers.

* [Hakyll.Core.Identifier.Pattern](/reference/Hakyll-Core-Identifier-Pattern.html)

    Combinators for creating *patterns*, i.e. predicates that pick out
    a set of resources: filesystem globs, arbitrary predicates,
    explicit lists, regular expressions, and so on.

* [Hakyll.Web.Page](/reference/Hakyll-Web-Page.html)

    A `Page`, consisting of some metadata and a body, is one of the
    most fundamental structures used by hakyll.  This module has some
    documentation explaining how pages work, and defines a number of
    compilers and utilities for working with them.

* [Hakyll.Web.Page.Metadata](/reference/Hakyll-Web-Page-Metadata.html)

    Utilities for manipulating page metadata.

* [Hakyll.Web.Template](/reference/Hakyll-Web-Template.html)

    Templates specify how to take the content of a `Page` and turn
    it into HTML output.

* [Hakyll.Main](/reference/Hakyll-Main.html)

    The main `hakyll` function that runs the whole show.  There is
    also a `hakyllWith` function which allows for a custom
    configuration.

## Pre-packaged solutions

These modules contain some "pre-packaged" solutions for common
situations.  They can be used as-is, or their source can be used as
inspiration for your own tools.

* [Hakyll.Web.Page.List](/reference/Hakyll-Web-Page-List.html)

    Combine several pages into a list, such as a list of posts, images
    in a gallery, etc.

* [Hakyll.Web.Feed](/reference/Hakyll-Web-Feed.html)

    Create RSS feeds.

* [Hakyll.Web.Tags](/reference/Hakyll-Web-Tags.html)

    Work with tags and categories.

## Useful utilities

* [Hakyll.Core.UnixFilter](/reference/Hakyll-Core-UnixFilter.html)

    Use any unix utility as a compiler.

* [Hakyll.Core.Util.Arrow](/reference/Hakyll-Core-Util-Arrow.html)

    A few utilities for working with arrows, including a constant
    arrow, unit arrow, and running an entire list of arrows on a
    single input.

* [Hakyll.Core.Util.File](/reference/Hakyll-Core-Util-File.html)

    Utility functions for working with the file system.

* [Hakyll.Core.Util.String](/reference/Hakyll-Core-Util-String.html)

    A few utility functions for working with `String`s (trim spaces,
    find and replace, split on regexp).

* [Hakyll.Core.Writable.WritableTuple](/reference/Hakyll-Core-Writable-WritableTuple.html)

    A utility type covering the situation where you generate some
    data, some of which you want to write to disk, and some of which
    you don't want to write but will be needed in order to generate
    something else later.

## Advanced customization

* [Hakyll.Core.Writable](/reference/Hakyll-Core-Writable.html)

    The `Writable` class is for resources which can be written to disk
    as part of the output site.  You can specify how to write your own
    custom types to disk by giving them a `Writable` instance.

* [Hakyll.Web.Pandoc](/reference/Hakyll-Web-Pandoc.html)

    Some compilers for running pandoc.  Normally they are run
    automatically as part of, for example, `pageCompiler`; but
    sometimes it is useful to be able to run pandoc explicitly.
