---
title: Installation
author: Jasper Van der Jeugt
type: main
---

Installation
------------

Installation is provided via Hackage, and some packages are available for
different distributions. For installation from source (i.e. via Hackage),
[stack] is recommended:

    $ stack install hakyll

[stack]: http://www.haskellstack.org/

Linux distro packages:

- [Debian unstable](http://packages.debian.org/source/sid/haskell-hakyll)
- [Fedora](https://apps.fedoraproject.org/packages/ghc-hakyll)
- [Nix](https://nixos.org/nixos/packages.html#hakyll): `$ nix-env -iA nixos.haskellPackages.hakyll` 

Building the example site
-------------------------

Apart from the main Hakyll library, the cabal package also provides you with an
executable `hakyll-init` to create an example site. This is an easy way to get
started:

    $ stack exec hakyll-init my-site

This creates a folder `my-site` in the current directory, with some example
content and a generic configuration.

If `hakyll-init` is not found, you should make sure your stack bin path
(usually `$HOME/.local/bin`) is in your `$PATH`. You can check your stack local
bin path by running `stack path --local-bin`.

Now, change into `my-site` directory and create a file `stack.yaml`
here with the following content:

    resolver: lts-14.16  # Adapt this as needed
    packages:
      - .
    extra-deps:
      - hakyll-4.13.0.1

On NixOS you will probably have to add the following lines to this
file:

    nix:
      enable: true
      packages: [zlib.dev, zlib.out]

The file `site.hs` holds the configuration of your site, as an executable
haskell program. We can compile and run it like this:

    $ stack build
    $ stack exec site build

If you installed `hakyll` with a preview server (this is the default), you can
now use

    $ stack exec site watch

and have a look at your site at
[http://localhost:8000/](http://localhost:8000/).
