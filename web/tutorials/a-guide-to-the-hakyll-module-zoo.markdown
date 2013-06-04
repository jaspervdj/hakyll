---
title: A Guide to the Hakyll Module Zoo
author: Brent Yorgey
---

The hakyll package [contains a bewildering array](/reference/) of modules, and
it's hard to know where to look when you are just getting started -- especially
since many of them are mostly just used by Hakyll internally and not that useful
to website authors. This guide provides a quick reference to the contents of the
most important and useful modules.

## Core modules

These are the modules containing the fundamental tools and concepts
you need to get started building a site with hakyll.

* [Hakyll.Core.Compiler](/reference/Hakyll-Core-Compiler.html)

    This is one of the modules you should look at first. It defines the
    fundamental `Compiler` type and primitive `Compiler`s as well as several
    variants on `load`, which allow you to bring together multiple resources to
    create a single output.

* [Hakyll.Core.Routes](/reference/Hakyll-Core-Routes.html)

    Specify where compiled items should go in the output site.

* [Hakyll.Core.Rules](/reference/Hakyll-Core-Rules.html)

    Specify which compilers and routes should be used on which resources. Also
    has combinators for versions (necessary if you want to use the same
    resources for multiple purposes).

* [Hakyll.Core.Identifier.Pattern](/reference/Hakyll-Core-Identifier-Pattern.html)

    Combinators for creating *patterns*, i.e. predicates that pick out a set of
    resources: filesystem globs, explicit lists, regular expressions, and so on.

* [Hakyll.Web.Page](/reference/Hakyll-Web-Pandoc.html)

    Provides functions for rendering your pages using pandoc, with a varying
    degree of high-levelness.

* [Hakyll.Web.Template](/reference/Hakyll-Web-Template.html)

    Templates specify how to take the content of an item and turn it into HTML
    output.

* [Hakyll.Web.Template.Context](/reference/Hakyll-Web-Template-Context.html)

    You can't do much with a `Template` if you don't have a `Context`: this
    module provides some predefined `Context`s and the tools to build your own.

* [Hakyll.Main](/reference/Hakyll-Main.html)

    The main `hakyll` function that runs the whole show.  There is also a
    `hakyllWith` function which allows for a custom configuration.

## Pre-packaged solutions

These modules contain some "pre-packaged" solutions for common situations. They
can be used as-is, or their source can be used as inspiration for your own
tools.

* [Hakyll.Web.Template.List](/reference/Hakyll-Web-Template-List.html)

    Combine several pages into a list, such as a list of posts, images in a
    gallery, etc.

* [Hakyll.Web.Feed](/reference/Hakyll-Web-Feed.html)

    Create RSS feeds.

* [Hakyll.Web.Tags](/reference/Hakyll-Web-Tags.html)

    Work with tags and categories.

## Useful utilities

* [Hakyll.Core.UnixFilter](/reference/Hakyll-Core-UnixFilter.html)

    Use any unix utility as a compiler.

* [Hakyll.Core.Util.String](/reference/Hakyll-Core-Util-String.html)

    A few utility functions for working with `String`s (trim spaces, find and
    replace, split on regexp).

## Advanced customization

* [Hakyll.Core.Writable](/reference/Hakyll-Core-Writable.html)

    The `Writable` class is for resources which can be written to disk as part
    of the output site.  You can specify how to write your own custom types to
    disk by giving them a `Writable` instance.
