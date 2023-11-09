# Getting Started

## Using Cabal

In the project directory, you can use `cabal new-install` to install the site executable (which builds your website). Alternatively, you can just use `cabal new-run site [command]`.

You can build the site using:

```sh
site build
```

And preview (and build) it using:

```sh
site watch
```

## Using Stack

The file `site.hs` holds the configuration of your site, as an executable haskell program. We can compile and run it like this:

```sh
stack build
stack exec site build
```

If you installed hakyll with a preview server (this is the default), you can now use:

```sh
stack exec site watch
```

## What's next?

Have a look at your site at <http://localhost:8000/> and explore tutorials that can be found on
[the hakyll homepage](http://jaspervdj.be/hakyll).
