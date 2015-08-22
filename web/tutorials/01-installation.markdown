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

- [Debian unstable](http://packages.debian.org/source/sid/haskell-hakyll)

Building the example site
-------------------------

Apart from the main Hakyll library, the cabal package also provides you with an
executable `hakyll-init` to create an example site. This is an easy way to get
started:

    $ hakyll-init my-site

This creates a folder `my-site` in the current directory, with some example
content and a generic configuration.

If `hakyll-init` is not found, you should make sure `$HOME/.cabal/bin` is in
your `$PATH`.

(If you're on OS X you may not have a bin directory in `$HOME/.cabal`. In this
case, check `$HOME/Library/Haskell/bin` and put it on your path if you find
`hakyll-init` there. See [here] for more information on installation paths on
OS X.)

[here]: http://www.haskell.org/haskellwiki/Mac_OS_X_Common_Installation_Paths

The file `site.hs` holds the configuration of your site, as an executable
haskell program. We can compile and run it like this:

    $ cd my-site
    $ ghc --make -threaded site.hs
    $ ./site build

If you installed `hakyll` with a preview server (this is the default), you can
now use

    $ ./site watch

and have a look at your site at
[http://localhost:8000/](http://localhost:8000/).
