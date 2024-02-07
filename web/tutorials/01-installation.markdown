---
title: Installation
author: Jasper Van der Jeugt
type: main
---

Installation
------------

Installation is provided via Hackage, and some packages are available for
different distributions.  There are a few different methods to install
Hakyll.

1.  You can use [ghcup] to install Cabal and then use:

        $ cabal new-install hakyll

2.  Using [stack]:

        $ stack install hakyll

3.  There are also some Linux distro packages:

    - [Debian](https://packages.debian.org/source/stable/haskell-hakyll)
    - [Fedora](https://apps.fedoraproject.org/packages/ghc-hakyll)
    - [Nix]: `$ nix-env -iA nixos.haskellPackages.hakyll`

[ghcup]: https://www.haskell.org/ghcup/
[Nix]: https://nixos.org/nixos/packages.html#hakyll
[stack]: http://www.haskellstack.org/

Building the example site
-------------------------

Apart from the main Hakyll library, the package also provides you with an
executable `hakyll-init` to create an example site.  This is an easy way to get
started.

Using cabal
===========

Create the example site:

    hakyll-init my-site

If `hakyll-init` is not found, make sure `~/.ghcup/env` is sourced.

In the `my-site` directory, you can use `cabal new-install` to install the
`site` executable (which builds your website).  Alternatively, you can just
use `cabal new-run site [command]`.

You can build the site using:

    site build

And preview (and build) it using:

    site watch

Using stack
===========

Create the `my-site` directory with the project files inside:

    $ stack exec hakyll-init my-site

Now, change into `my-site` directory and run `stack init` to create the
`stack.yaml` file.

The file `site.hs` holds the configuration of your site, as an executable
haskell program. We can compile and run it like this:

    $ stack build
    $ stack exec site build

If you installed `hakyll` with a preview server (this is the default), you can
now use:

    $ stack exec site watch

and have a look at your site at
[http://localhost:8000/](http://localhost:8000/).

### NixOS

Make sure stack knows how to access your system's locale (or you might run into errors when building):

    stack --nix exec -- locale

If the variables listed under this command are not set appropriately (i.e. `LANG="POSIX"`, etc.), make sure to run stack from a shell that does assign them as needed. For example in your project's `shell.nix`:

```nix
shellHook = ''
  export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
  export LANG=en_US.UTF-8
'';
```

Furthermore, even though stack's integration with Nix on NixOS is mostly reliable you might have to add the following lines to your `stack.yaml` file:

    nix:
      enable: true
      packages: [zlib.dev, zlib.out]
