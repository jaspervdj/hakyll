---
title: FAQ
author: Jasper Van der Jeugt
---

## "File name does not match module name" on Mac OS

    Hakyll.hs:1:1:
        File name does not match module name:
        Saw: `Main'
        Expected: `Hakyll'

Is an error encountered on Mac OS when your configuration is named `hakyll.hs`
and located on a case-insensitive filesystem. A workaround is to rename it to
something that isn't the name of the module, for example, `site.hs`.

## pandocCompiler/Hakyll/Pandoc eats my HTML!

Sometimes, it can seem that HTML pages are stripped of some arbitrary tags, e.g.
`<div>`'s. The issue here is that, when using the default `pandocCompiler`, your
page passes through Pandoc. Pandoc unfortunately strips away this information,
giving you the "wrong" HTML.

The solution is not to use `pandocCompiler`, but something simpler like
`getResourceBody`. This way, your HTML is not touched.

## Does Hakyll support syntax highlighting?

Syntax highlighting is enabled by default in Hakyll if you are using a somewhat
recent version of Pandoc (1.9 and onwards). Note that you also need to include
some CSS in order for this to work! This site, for example, uses the [default
Pandoc syntax CSS file][syntax-css].

[syntax-css]: https://github.com/jaspervdj/hakyll/blob/master/web/css/syntax.css

## When should I rebuild and when should I build?

If you execute a `./site build`, Hakyll will build your site incrementally.
However, we can not detect if you edited `site.hs`. In this case, you first want
to compile it again `site.hs` again, and then do a `./site rebuild`.

After rebuilding your site, all files will look as "modified" to the filesystem.
This means that when you upload your site, it will usually transfer all files --
this can generate more traffic than necessary, since it is possible that some
files were not actually modified. If you use `rsync`, you can counter this using
the `--checksum` option.
