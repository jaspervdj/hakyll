---
title: Releases
---

# Releases

## Hakyll 4.4.1.0

- Use Pandoc 1.12 highlighting by default

## Hakyll 4.4.0.0

- Update to work with Pandoc 1.12. This changes the type of `readPandocBibilio`:
  the `CSL` argument is no longer optional (contribution by Jorge Israel Peña)

- Fix incorrect output of `toSiteRoot` on windows (contribution by Saeid
  Al-Wazzan)

- Add a preview port option to `Configuration` (contribution by Jorge Israel
  Peña)

- Add `watch` command that polls for changes but does not necessarily launch a
  server (contribution by Eric Stolten)

- Generalise type of `metadataField`

- Fix issue where metadata was not correctly loaded when using versions

## Hakyll 4.3.3.0

- Re-add the `functionField` function

## Hakyll 4.3.2.0

- Re-add the `mapContext` function

- Unescape internal URLs when using `./site check` (contribution by Marc-Antoine
  Perennou)

## Hakyll 4.3.1.0

- Make teasers undefined if no `<!--more-->` comment is found

- Sanitize tag URLs (contribution by Simonas Kazlauskas)

## Hakyll 4.3.0.0

- Add conditionals, partials and for loops to the template system (includes a
  contribution by Ivan N. Veselov)

- Improvements to the preview functionality on windows (contribution by Jorge
  Israel Peña)

- Add pagination support (contribution by Anton Dubovik)

- Slight speedup for the Hakyll cache (contribution by justnoxx)

- Add teaser functionality (contribution by Ivan N. Veselov)

- Make `./site check` work with scheme-relative URLs (contribution by Simonas
  Kazlauskas)

- The `./site deploy` command can now be customized with Haskell code
  (contribution by Samuel Tardieu)

- Use `hsnotify` for proper polling instead of sleep loop on all platforms
  (contribution by Simonas Kazlauskas)

- More useful debug info available

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
