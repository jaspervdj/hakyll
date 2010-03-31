---
title: Interlude
what: gives some various tips and tricks about Hakyll
---

## Auto-compilation

Hakyll features a simple _auto-compilation_ mode. This is invoked by running

~~~~~
[jasper@alice ~]$ ./hakyll preview
Starting hakyll server on port 8000...
~~~~~

Now, Hakyll will recompile your site when you change files, so you can just
refresh in your browser. There is one more thing to note: this will not update
your site automatically when `hakyll.hs` changes. So if you make any changes to
the configuration file, you'll have to compile it again, and then you can enter
`preview` mode again.

## When to rebuild

If you execute a `./hakyll build`, Hakyll will build your site incrementally.
This means it will be very fast, but it will not pick up _all_ changes.

- In case you edited `hakyll.hs`, you first want to compile it again.
- It is generally recommended to do a `./hakyll rebuild` before you deploy your
  site.

## Pretty URL's

There is an option in Hakyll to produce pretty URL's, which is disabled by
default because it can be confusing when you're first introduced to Hakyll.

It can be enabled this way:

~~~~~{.haskell}
import Text.Hakyll
import Text.Hakyll.Hakyll

myHakyllConfiguration :: HakyllConfiguration
myHakyllConfiguration = defaultHakyllConfiguration
    { enableIndexUrl = True
    }

main = hakyllWithConfiguration HakyllConfiguration $ do
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

## The gist of it

- Auto-compilation is handy, but a rebuild is recommended before deploying your
  site.
- You can enable pretty URL's in Hakyll. It is, however, not the default.
