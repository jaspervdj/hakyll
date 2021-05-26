---
title: Home
---

# Overview

Static sites are fast, secure, easy to deploy, and manageable using version
control.

Hakyll is a [Haskell](http://haskell.org) library for generating static sites,
mostly aimed at small-to-medium sites and personal blogs. It is written in a
very configurable way and uses an [xmonad](http://xmonad.org)-like DSL for
configuration.

Integration with [pandoc](http://johnmacfarlane.net/pandoc/) gives us markdown
and TeX support, including syntax highlighting and other goodies.

# The Hakyll System

## Write your content in whatever format you prefer

![Write your content](/images/hakyll-system-1.png)

## Create compilation rules in a Haskell EDSL

![Write your rules](/images/hakyll-system-2.png)

## Compile it to HTML and upload it!

![Compile it](/images/hakyll-system-3.png)

# Getting Started

You can get the latest version from hackage using `cabal install hakyll`, or
using [stack] by using `stack install hakyll`. Then, you can:

[stack]: http://www.haskellstack.org/

- read the [tutorials](/tutorials.html);
- mail the [google discussion group](http://groups.google.com/group/hakyll);
- ask questions on the IRC channel: `#hakyll` on
  [irc.libera.chat](https://libera.chat/) (we *do not* have a channel on
  Freenode anymore).
