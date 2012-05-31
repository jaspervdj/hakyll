---
title: FAQ
author: Jasper Van der Jeugt
---

## "File name does not match module name" on Mac OS

    Hakyll.hs:1:1:
        File name does not match module name:
        Saw: `Main'
        Expected: `Hakyll'

Is an error encountered on Mac OS when `hakyll.hs` is located on a
case-insensitive filesystem. A workaround is to rename it to something that
isn't the name of the module, for example, `site.hs`.

## `pageCompiler`/Hakyll/Pandoc eats my HTML!

Sometimes, it can seem that HTML pages are stripped of some arbitrary tags, e.g.
`<div>`'s. The issue here is that, when using the default `pageCompiler`, your
page passes through Pandoc. Pandoc unfortunately strips away this information,
giving you the "wrong" HTML.

The solution is not to use `pageCompiler` -- it is very common to write custom
page processing compiler. The definition of `pageCompiler` is, put simply:

~~~~~{.haskell}
pageCompiler =
   readPageCompiler >>>
   addDefaultFields >>>  -- Sets some things like $path$
   arr applySelf    >>>  -- Used to fill in $var$s in the page
   pageRenderPandoc      -- Pass through pandoc
~~~~~

You can add your own version in your `hakyll.hs` file:

~~~~~{.haskell}
myPageCompiler =
   readPageCompiler >>>
   addDefaultFields >>>  -- Sets some things like $path$
   arr applySelf    >>>  -- Used to fill in $var$s in the page
~~~~~

And using this instead of `pageCompiler` should solve the issue.

## Does Hakyll support syntax highlighting?

Syntax highlighting is enabled by default in Hakyll if you are using a somewhat
recent version of Pandoc (1.9 and onwards). Note that you also need to include
some CSS in order for this to work! This site, for example, uses the [default
Pandoc syntax CSS file][syntax-css].

[syntax-css]: https://github.com/jaspervdj/hakyll/blob/master/web/css/syntax.css

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
