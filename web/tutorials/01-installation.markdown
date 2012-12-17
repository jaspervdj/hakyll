---
title: Installation
author: Jasper Van der Jeugt
---

Installation
------------

Installation is provided using [cabal], and some packages are available for
different distributions.

    $ cabal install hakyll

[cabal]: http://www.haskell.org/cabal/

If you have a recent installation of `cabal` and your time is somewhat valuable,
use:

    $ cabal install -j hakyll

Linux distro packages:

- [Archlinux (AUR)](https://aur.archlinux.org/packages/haskell-hakyll/)
- [Debian unstable](http://packages.debian.org/source/sid/haskell-hakyll)

Building the example site
-------------------------

Apart from the main Hakyll library, the cabal package also provided you with an
executable `hakyll-init` to create an example site. This is an easy way to get
started:

    $ hakyll-init my-site

If `hakyll-init` is not found, you should make sure `$HOME/.cabal/bin` is in
your `$PATH`.

The file `site.hs` holds the configuration of your site, as an executable
haskell program. We can compile and run it like this:

    $ cd my-site
    $ ghc --make site.hs
    $ ./site build

If you installed `hakyll` with a preview server (this is the default), you can
now use

    $ ./site preview

and have a look at your site at
[http://localhost:8000/](http://localhost:8000/).
