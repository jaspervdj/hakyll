---
title: Releases
---

# Releases

## Hakyll 4.2.2.0

- Fix issue with `Alternative` instance of `Compiler`

## Hakyll 4.2.1.1

*March 9, 2013*

- Make `http-conduit` dependency optional by adding a `checkExternal` cabal flag

## Hakyll 4.2.1.0

*March 7, 2013*

- Fix issue where `copyFileCompiler` ignored `providerDirectory`

## Hakyll 4.2.0.0

*March 7, 2013*

- Read second extension for `.lhs`, e.g. `.md.lhs` or `.tex.lhs` (contribution
  by Alexander Vershilov)

- Speedup initialization by using modification times instead of hashing files

- Speedup initialization with a rewritten resource provider

- Fix `./site check` not working with sites that require a user agent (e.g.
  <http://www.wikipedia.org/>)

- Change `chronological` and `recentFirst` to actually look at the dates of
  items. This changes their types from:

        chronological, recentFirst :: [Item a] -> [Item a]

    to:

        chronological, recentFirst
            :: MonadMetadata m => [Item a] -> m [Item a]

    (contribution by Simonas Kazlauskas)

- Add `metadataRoute`, so it is now possible to use metadata when determining
  routes

- Improve metadata parser for multiline metadata fields (contribution by Peter
  Jones)

- Add the `getMetadataField` utility

## Hakyll 4.1.4.0

*January 26, 2013*

- Export the flexible `renderTags` function

## Hakyll 4.1.3.0

*January 26, 2013*

- Export the constructor of the `Tags` datatype

## Hakyll 4.1.2.0

*January 20, 2013*

- Fix an issue where a dependency cycle would lead to infinite recursion/stack
  overflow

## Hakyll 4.1.1.0

*January 20, 2013*

- Fix an issue regarding `relativizeUrls` expanding `<meta />` to
  `<meta></meta>`

## Hakyll 4.1.0.0

*January 20, 2013*

Update to use Pandoc 1.10, this requires changes to your `site.hs` if you're
using custom Pandoc options or the `Hakyll.Web.Pandoc.Biblio` module.

- `defaultHakyllParserState` renamed to `defaultHakyllReaderOptions`

- The type of `readPandocBiblio` changed

Because of the many changes, this release is no longer compatible with Pandoc
1.9.

## Hakyll 4.0.0.0

*January 16, 2013*

The Initial release of Hakyll 4, see
[this blogpost](http://jaspervdj.be/posts/2013-01-16-hakyll-4.0.html) and
[the migration guide](/tutorials/hakyll-3-to-hakyll4-migration-guide.html) for
an overview of changes.
