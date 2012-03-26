---
title: Installation
author: Jasper Van der Jeugt
---

Why static websites?
--------------------

Modern web frameworks make it easy to create huge dynamic websites. Why would
anyone still care about a static website?

- Static websites are fast, because it's simply files served directly from the
  hard disk.
- Static websites are secure. Nobody has ever found an SQL injection in static
  pages.
- Static websites are easy to deploy. Just copy them to your webhost using
  (S)FTP/rsync/scp and you are done. They work on all webhosts: no CGI or extra
  modules needed for the web server.

Why Hakyll?
-----------

Hakyll is a [Haskell] library meant for creating small-to-medium sized static
websites. It is a powerful publishing tool, precisely because of the power of
Haskell. By using the awesome [pandoc] library, it is able to create your
website from a large variety of input formats.

[Haskell]: http://haskell.org/
[pandoc]: http://johnmacfarlane.net/pandoc/

Features include:

- easy templating system;
- a simple HTTP server for previewing and compiling your website on the go;
- powerful syntax highlighting;
- modules for common items such as tags and feeds;
- easily extensible.

Installation
------------

Installation is provided using [cabal], and some packages are available for
different distributions.

    cabal install hakyll

[cabal]: http://www.haskell.org/cabal/
