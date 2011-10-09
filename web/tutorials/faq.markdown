---
title: FAQ
author: Jasper Van der Jeugt
---

## Does Hakyll support syntax highlighting?

Syntax highlighting is enabled by default in Hakyll. However, you also need to
enable it in pandoc. If no syntax highlighting shows up, try

    [jasper@phoenix] cabal install --reinstall -fhighlighting pandoc

## When should I rebuild and when should I build?

If you execute a `./hakyll build`, Hakyll will build your site incrementally.
This means it will be very fast, but it will not pick up _all_ changes.

- In case you edited `hakyll.hs`, you first want to compile it again.
- It is generally recommended to do a `./hakyll rebuild` before you deploy your
  site.

After rebuilding your site, all files will look as "modified" to the filesystem.
This means that when you upload your site, it will usually transfer all files --
this can generate more traffic than necessary, since it is possible that some
files were not actually modified. If you use `rsync`, you can counter this using
the `--checksum` option.

## Problem with regex-pcre dependency on Mac OS

Hakyll requires [regex-pcre], which might fail to build on Mac OS. To solve
this problem, make sure the [pcre] C library is installed (via homebrew or
macports). Then install [regex-pcre] using:

    cabal install --extra-include-dirs=/usr/local/include regex-pcre

or

    cabal install --extra-include-dirs=/opt/local/include regex-pcre

...and proceed to install Hakyll the regular way.

[regex-pcre]: http://hackage.haskell.org/package/regex-pcre
[pcre]: http://www.pcre.org/

## "File name does not match module name" on Mac OS

    Hakyll.hs:1:1:
        File name does not match module name:
        Saw: `Main'
        Expected: `Hakyll'

Is an error encountered on Mac OS when `hakyll.hs` is located on a
case-insensitive filesystem. A workaround is to rename it to something that
isn't the name of the module, for example, `site.hs`.
