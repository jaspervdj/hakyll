---
title: Interlude
what: gives some various tips and tricks about Hakyll (quite handy, read this!)
---

## Syntax-highlighting

Pandoc (which Hakyll uses as a backend) offers powerful syntax highlighting.
To enable this, Pandoc needs to be compiled with highlighting support. If this
is not the case, you can fix this using:

~~~~~
[jasper@alice ~]$ cabal install --reinstall -fhighlighting pandoc
~~~~~

## Auto-compilation

Hakyll features a simple _auto-compilation_ mode. This is invoked by running

~~~~~
[jasper@alice ~]$ ./hakyll preview
Starting hakyll server on port 8000...
~~~~~

Now, Hakyll will recompile your site when you refresh in your browser. This will
not update your site automatically when `hakyll.hs` changes. So if you make any
changes to the configuration file, you'll have to compile it again, and then you
can enter `preview` mode again.

If you use a custom `HakyllConfiguration`, you can select your custom
`PreviewMode`:

- `BuildOnRequest`: rebuild site when the preview server receives a request
  (default).
- `BuildOnInterval`: build when you change files.

## When to rebuild

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

## Pretty URL's

There is an option in Hakyll to produce pretty URL's, which is disabled by
default because it can be confusing when you're first introduced to Hakyll.

It can be enabled this way:

~~~~~{.haskell}
import Text.Hakyll
import Text.Hakyll.HakyllMonad

myConfig :: HakyllConfiguration
myConfig = (defaultHakyllConfiguration "http://jaspervdj.be")
    { enableIndexUrl = True
    }

main = hakyllWithConfiguration myConfig $ do
    -- Further code here
~~~~~

The effect will be that the internal `toUrl` function will behave differently.
A few examples:

- `about.html` will be rendered to `about/index.html`.
- `posts/2010-02-16-a-post.markdown` will be rendered to
  `posts/2010-02-16-a-post/index.html`.
- However, `index.markdown` will still be rendered to `index.html`. Likewise,
  `posts/index.html` would be rendered to `posts.index.html`.

The benefit of this is simply prettier URL's. That is, if you consider
`example.com/about` prettier than `example.com/about.html`.

## Default values

At some point, you might want to use a number of global key-value pairs, for
example, `$author`. There are two possible ways to achieve this.

- There is an option in `HakyllConfiguration` supporting this, called
  `additionalContext`. For an example on how to use `HakyllConfiguration`, see
  the pretty URL's section above.

- Another option is to use a `defaults.markdown` file, simply containing some
  metadata, and then `combine` this file with other pages. The advantage is
  that autocompilation mode will pick up changes in this file[^1].

[^1]: Original idea by zenzike.
