---
title: FAQ
author: Jasper Van der Jeugt
---

## "hGetContents: invalid argument" or "commitBuffer: invalid argument"

If you get any of the errors:

    commitBuffer: invalid argument (invalid character)

or:

    hGetContents: invalid argument (Invalid or incomplete multibyte or wide
        character)

It means that your Hakyll executable couldn't write to (in the former case) or
read (in the latter) from an UTF-8 encoded file.

On most linux distros, you can solve this by setting your `LANG` to use UTF-8,
using something like:

    LANG=nl_BE.UTF-8 ./site build

You should also add this to your `.profile`, or whatever config file you use.

On Windows, running `chcp 65001` before running your Hakyll executable has been
reported to work.

Alternatively, you can specify this in your `site.hs`:

```haskell
import qualified GHC.IO.Encoding as E

main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    hakyll $ do
        ...
```

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

To highlight a code block, you need to use Pandoc's fenced code block syntax to
set the block's language. For example, here's how you highlight Haskell code:

    ``` haskell
    fac n = foldr (*) 1 [1..n]
    ```

For details, see Pandoc's user guide on [fenced code
blocks][pandoc-code-blocks] and [inline code][pandoc-inline-code].

[syntax-css]: https://github.com/jaspervdj/hakyll/blob/master/web/css/syntax.css
[pandoc-code-blocks]: http://johnmacfarlane.net/pandoc/README.html#fenced-code-blocks
[pandoc-inline-code]: http://johnmacfarlane.net/pandoc/README.html#verbatim

## When should I rebuild and when should I build?

If you execute a `./site build`, Hakyll will build your site incrementally.
However, we can not detect if you edited `site.hs`. In this case, you first want
to compile `site.hs` again, and then do a `./site rebuild`.

After rebuilding your site, all files will look as "modified" to the filesystem.
This means that when you upload your site, it will usually transfer all files --
this can generate more traffic than necessary, since it is possible that some
files were not actually modified. If you use `rsync`, you can counter this using
the `--checksum` option.
