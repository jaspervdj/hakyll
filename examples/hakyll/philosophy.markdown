---
title: Philosophy
---

## Small-to-medium sites

Hakyll was written to be used for small-to-medium sites. You can do some
advanced things with it, but don't use it to build a big online shop.

## Hakyll.hs

It should be possible to put all configuration in one file, so data and
configuration can be strictly separated. In addition, we think this file should
never exceed a 100 lines of code.

## High-level

Hakyll tries to provide as many high-level functions as possible for common
tasks, while the lower-level functions should also be accessible. If you think
you're writing something that can be used for many sites, please send a patch,
or your `hakyll.hs`, and we will see what we can do.

## Well-documented

A key to being easy-to-use is documentation. That's why we try to provide as
many working examples as possible. If you ever create a site using hakyll,
please consider open-sourcing it, as people might be able to learn from your
code.
