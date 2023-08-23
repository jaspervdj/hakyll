---
title: Releases
---

# Releases

## Hakyll 4.16.0.0 (2023-04-27)

- Bump `base` *lower* bound to 4.12 (GHC >= 8.6). Hakyll already failed to build
    on earlier versions due to the template-haskell requirement, and nobody
    complained about that, so I assume nobody cares if the support is properly
    dropped (contribution by Alexander Batischev)
- Export `Hakyll.Tags.simpleRenderLink` (contribution by Alexander Batischev)
- Add `Hakyll.Web.Pandoc.Biblio.pandocBibliosCompiler` to load multiple bib
    files by glob (contribution by Liang-Ting Chen)
- Fix "Store.set: resource busy" error (contribution by Jasper Van der Jeugt)
- Teach `Hakyll.Web.Template.Context.getItemUTC` about another date format,
    "%d.%m.%Y" (contribution by dukzcry)
- Add `Hakyll.Web.Html.withTagListM`, a monadic version of `withTagList`
    (contribution by 0xd34df00d)
- Fix all the warnings and enable `-Werror` in CI (contribution by Alexander
    Batischev)
- Miscellaneous updates and fixes to the docs (contributions by Tony Zorman,
    Alexander Batischev, malteneuss, Agustín Mista, Martin Bukatovič, Muhammad
    Aviv Burhanudin, Jacek Galowicz, Daniel Mlot, Yoo Chung, Robert Pearce)
- Export `Hakyll.defaultCommands`, i.e. Hakyll's set of commands (`build`,
    `check`, `clean` etc.) (contribution by Alexander Batischev)
- Add a `Hakyll.Core.Configuration.Configuration.previewSettings` field which
    lets the user override the settings used by the preview server (contribution
    by Christopher League and Brian McKenna)
- Make email address in RSS/Atom feeds optional (just set it to an empty string)
    (contribution by Robert)
- `Hakyll.Web.Meta.TwitterCard`: use `name` instead of `property` for better
    spec compliance (contribution by ncaq)
- Add a `Hakyll.Core.Configuration.Configuration.checkHtmlFile` predicate which
    dictates what files will get link-checked by the `check` command. Default
    predicate accepts files with .html and .xhtml extensions (contribution by
    Yoo Chung, with earlier contribution from Michael Orlitzky)
- Add support for GHC 9.2 (contribution by Laurent P. René de Cotret)
- Bump `optparse-applicative` upper bound to allow 0.17 (contribution by
    Alexander Batischev)
- Bump `pandoc` upper bound to allow 2.19 (contribution by Alexander Batischev)
- Allow `text` 2.0 (contribution by Alexander Batischev)
- Bump `vector` upper bound to allow 0.13 (contribution by Alexander Batischev)
- Allow `aeson` 2.1 (contribution by Alexander Batischev)
- Bump `fsnotify` upper bound to allow 0.4 (contribution by Alexander Batischev)
- Bump `resourcet` upper bound to allow 1.3 (contribution by Alexander
    Batischev)
- Bump `template-haskell` upper bound to 2.20 (GHC 9.4.3) (contribution by
    Alexander Batischev)
- Allow `pandoc` 3.0. Note that the behavior of Hakyll's `readPandocBiblios` and
    `readPandocBiblio` is different whether pandoc 2 or 3 is installed
    (contribution by Laurent P. René de Cotret) 
- Bump `mtl` upper bound to allow 2.3 (contribution by Alexander Batischev)
- Bump `pandoc` upper bound to allow 3.1 (contribution by Laurent P. René de
    Cotret)
- Bump `template-haskell` upper bound to 2.21 and `time` to 1.12 (GHC 9.6.1)
    (contribution by Laurent P. René de Cotret)

## Hakyll 4.15.1.1 (2022-01-20)

- Extend the documentation for `Hakyll.Core.Identifier` (contribution by
    malteneuss)
- Fix yet another regression caused by new dependency checking code
    (contribution by Laurent P. René de Cotret)
- Bump `pandoc` upper bound to allow 2.17 (contribution by Alexander Batischev)
- Website now points to Hackage rather than its own (often outdated) version of
    the docs (contribution by Jasper Van der Jeugt)

## Hakyll 4.15.1.0 (2021-10-25)

- Add `Hakyll.Web.Pandoc.Biblio` functions `readPandocBiblios` and
    `processPandocBiblios`, which let one use multiple bibliographies
    (contribution by Benjamin Eskola)
- Preserve file extension of bibliography files when passing them to Pandoc.
    This enables one to use not just BibTex, but also YAML and JSON files
    (contribution by Benjamin Eskola)
- Fix URL extraction for `srcset` attribute. This affects
    `Hakyll.Web.HTML.relativizeUrls` and other such functions (contribution by
    Alexander Batischev)
- Bump `bytestring` upper bound to allow 0.11 (contribution by Alexander
    Batischev)
- Bump `pandoc` upper bound to allow 2.15 (contribution by Alexander Batischev)
- Bump `aeson` bounds to allow 2.0 (contribution by Alexander Batischev)

## Hakyll 4.15.0.1 (2021-10-02)

- Add missing test file to the package (contribution by Alexander Batischev)

## Hakyll 4.15.0.0 (2021-10-01)

- Fix dependency cycles detector (contribution by Laurent P. René de Cotret)
- Add `--dry-run` to the `build` command (contribution by Fraser Tweedale)
- Add support for Jupyter notebooks (files with ".ipynb" extension)
    (contribution by fedeinthemix)
- Add `Hakyll.Web.Pandoc.Biblio.processPandocBiblio`, which works with `Item
    Pandoc` and is thus composable with other Pandoc-related compilers and
    functions (contribution by fedeinthemix)
- Speed up the runtime by about 9% (multiple contributions by Fraser Tweedale)
- Tolerate unexpected cache misses by recompiling the item; now there is no need
    to rebuild the entire site to fix cache corruption (contribution by Fraser
    Tweedale)
- Replace `Hakyll.Core.Rules.forceCompile` (introduced in 4.14.1.0) with
    `Hakyll.Core.Compiler.recompilingUnsafeCompiler`. The new facility is more
    versatile and composes better (contribution by Fraser Tweedale)
- Remove dependency on `array` (contribution by Laurent P. René de Cotret)

## Hakyll 4.14.1.0 (2021-08-30)

- Add `Hakyll.Web.Html.demoteHeaderBy` function, which demotes an HTML header by
    a given amount (contribution by Logan McGrath)
- Add `Hakyll.Core.Rules.forceCompile` modifier which forces re-compilation of
    an item even if its file wasn't modified. This is useful for data sources
    which aren't local files (contribution by Fraser Tweedale)
- Add `Hakyll.Web.Tags.getTagsByField` which extracts tags from a given field
    instead of the default "tags" field (contribution by Jim McStanton and
    Alexander Batischev)
- Add `Hakyll.Core.Configuration.shouldWatchIgnore` function and the
    corresponding `watchIgnore` field for `Configuration`. These are used to
    ignore files in watch mode, which is useful when certain files are
    pre-processed and the results are saved into the provider directory
    (contribution by Aron Erben)
- Add `Hakyll.Web.Meta` modules `JSONLD`, `OpenGraph`, and `TwitterCard`. These
    help with semantic web metadata in pages. This adds dependency on `aeson`
    (contribution by Fraser Tweedale)
- Export `Hakyll.Web.Pandoc.Biblio.unCSL` function (contribution by Benjamin
    Bray)
- Make the runtime concurrent, which brings 30% speedups on real-world sites.
    This adds dependencies on `array` and `lifted-async`. Please note that it
    doesn't scale past the number of physical cores; ideas are welcome in
    https://github.com/jaspervdj/hakyll/issues/850 (contribution by
    Laurent P. René de Cotret and Vaibhav Sagar)
- Fix binary's name in the first tutorial (contribution by Alexander Batischev)
- Fix "Empty 'do' block" error in GitHub tutorial (contribution by
    alexandroid000)
- Move #hakyll IRC channel from Freenode to Libera.Chat (by Alexander Batischev
    and henk)
- Replace dependency on `cryptonite` and `memory` with dependency on `hashable`
    (contribution by Laurent P. René de Cotret)
- Bump `file-embed` upper bound to 0.0.15 (contribution by Alexander Batischev)
- Bump `optparse-applicative` upper bound to 0.16 (contribution by Felix Yan)
- Bump `pandoc` upper bound to 2.14 (contribution by Laurent P. René de Cotret)
- Bump `tasty` upper bound to 1.4 (contribution by Felix Yan)
- Bump `template-haskell` upper bound to 2.17, which is shipped with GHC 9
    (contribution by Alexander Batischev)
- Bump `time` upper bound to 1.11 (contribution by Alexander Batischev)

## Hakyll 4.14.0.0 (2021-03-14)

- Add `renderPandocWithTransform` and `renderPandocWithTransformM` (by Norman
  Liu)
- Make sure the initial project is writable (by Tobias Bora)
- Bump `pandoc` to 2.11.*
- Bump `file-embed` upper bound to 0.0.14
- Bump `random` upper bound to 1.2

## Hakyll 4.13.4.1 (2020-09-30)

- Bump `pandoc` to 2.10.*
- Bump upper bound for `template-haskell` to 2.17
- Bump `QuickCheck` upper bound to 2.15

## Hakyll 4.13.4.0 (2020-06-20)

- Miscellaneous Windows-specific fixes and CI (by Laurent P. René de Cotret)
- Bump upper bound for `cryptonite` to 0.28
- Bump upper bound for `tasty` to 1.4

## Hakyll 4.13.3.0 (2020-04-12)

- Fix compilation issue related to `MonadFail` on Windows (by Martín Emanuel)
- Bump upper bound for `warp` to 3.4
- Bump upper bound for `pandoc-citeproc` to 0.18

## Hakyll 4.13.2.0 (2020-03-07)

- Fix compatibility with GHC-8.6 (by Nikolay Yakimov).

## Hakyll 4.13.1.0 (2020-02-26)

- Fix timezone parsing bug with time-1.9
- Remove constant field for homepage title in example site (by Liang-Ting Chen)
- Clean up `stack.yaml` (by Hexirp)
- Use crytonite instead of cryptohash (by Hexirp)
- Expose CLI argument parser internals (by Jim McStanton)
- Support GHC-8.8. Add `MonadFail` instances and constraints (by Veronika
  Romashkina)
- Fix file path compatibility with Windows (by Hexirp)
- Fix logging output flushing in `site server` (by robx)
- Fix spacing of command line usage in `hakyll-init` (by robx)
- Add titles to tag fields by default

## Hakyll 4.13.0.1 (2019-09-18)

- Add missing test files (contribution by Justin Humm)

## Hakyll 4.13.0.0 (2019-08-30)

- Improved documentation in many places (contribution by Bergi)
- Significantly improve error messages when reading and applying templates
  (contribution by Bergi)
    * `empty` and `fail` for `Compiler` now fail without or with a message,
      allowing for much better debug output
    * `renderFeed`, `renderRssWithTemplates` and `renderAtomWithTemplates` now
      take `Template` rather than `String` arguments
- Add option to specify date in directory structure,e.g.
  `posts/2019/05/10/tomorrow.md` (contribution by Taeer Bar-Ya)
- Bump QuickCheck to 2.13

## Hakyll 4.12.5.2 (2019-05-09)

- Bump pandoc to 2.7
- Add `srcset` to the list of URL attributes (contribution by c50a326)
- Expose `getCategory` in `Hakyll.Web.Tags` (contribution by Ng Wei En)
- Fix issue where `published` overwrites the user's context (contribution by
  ncaq)

## Hakyll 4.12.5.1 (2019-02-03)

- Bump pandoc to 2.6
- Bump pandoc-citeproc to 0.16

## Hakyll 4.12.5.0 (2019-01-12)

- Update dependencies (contribution by Hexirp):
    * Bump containers to 0.6
    * Bump yaml to 0.11
    * Bump pandoc to 2.5
    * Bump pandoc-citeproc to 0.15
    * Bump tasty to 1.2
- Correct assertion in unixFilterError test
  (contribution by Jim McStanton)
- Add renderRssWithTemplates, renderAtomWithTemplates
  (contribution by Abhinav Sarkar)
- Speed up hashing in cache (contribution by 0xd34df00d)
- Update type of fromFilePath to use FilePath instead of String
  (contribution by Jim McStanton)
- Drop extension when parsing dates in filepaths (contribution by
  Gabriel Aumala)

## Hakyll 4.12.4.0 (2018-08-13)

- Bump yaml to 0.10
- Bump file-embed to 0.0.11
- Bump QuickCheck to 2.12
- Use makeRelativeToProject with embedFile for stack ghci (contribution by
  Michael Sloan)

## Hakyll 4.12.3.0 (2018-05-29)

- Bump tasty to 1.1
- Bump fsnotify to 0.3

## Hakyll 4.12.2.0

- Bump pandoc to 2.2
- Fix path resolution in Pandoc.Biblio (contribution by knih)

## Hakyll 4.12.1.0

- Fix hakyll-init on older GHC versions
- Make the Pandoc dependency optional

## Hakyll 4.12.0.1

- Bump resourcet to 1.2 in test section

## Hakyll 4.12.0.0

- Add Semigroup instances for existing Monoids (contribution by
  Christian Barcenas)
- Fix issue with CPP and comment containing `/*`
- Add `withTagList` (contribution by Oleg Grenrus)
- Improve CSS compression (contribution by Bergi)
- Bump pandoc-citeproc to 0.14
- Bump resourcet to 1.2
- Bump time to 1.9
- Bump http-types to 0.12
- Bump http-conduit to 2.3
- Bump tasty-quickcheck to 0.10

## Hakyll 4.11.0.0

- Bump binary to 0.9
- Bump pandoc to 2.1
- Bump pandoc-citeproc to 0.13
- Bump http-types to 0.11
- Bump QuickCheck to 2.11
- Bump tasty to 1.0
- Bump tasty-hunit to 0.10
- Remove system-filepath dependency
- Embed feed templates rather than using data-files (contribution by Roman
  Kuznetsov)
- Fix pthread link error on GHC-8.2.2 (contribution by Shinya Yamaguchi)
- Extend capture with Regex handling (contribution by frederik-h)

## Hakyll 4.10.0.0

- Bump Pandoc to 2.0 (contribution by Vaibhav Sagar)
- Fix compression of calc() in CSS (contribution by Krzysztof Jurewicz)
- Make unixFilter output stderr when failing (contribution by Nick Boultbee)
- Export Check type from `Hakyll.Commands` (contribution by Futtetennista)
- Add overwritten files check to `hakyll-init` (contribution by Ilya Murzinov)
- Expose & document `hakyllWithExitCodeAndArgs`, `Options`, and `Command`
  (contribution by Michael Walker)
- Add check for non-overlapping redirects (contribution by gwern)
- Fix early exit when calling check as a library

## Hakyll 4.9.8.0

- Bump pandoc-citeproc to 0.10.5
- Bump optparse-applicative to 0.14
- Bump QuickCheck to 2.10
- Bump tasty-quickcheck to 0.9
- Restructure .cabal to avoid redundant compilation (contribution by
  Christopher League)

## Hakyll 4.9.7.0

- Fix compilation trouble with `Options.Applicative`
- Some small CSS compression improvements (contribution by Nicole Rauch)

## Hakyll 4.9.6.0

- Tighten dependency on `pandoc-citeproc` (contribution by Mikhail Glushenkov)
- Enable using a custom parser for command line arguments (contribution by
  Alberto)
- Update examples to semantic HTML (contribution by Elie Génard)
- Better error for `cached` on non-existing file
- Provide an `$allPages$` key when doing pagination
- Preserve file metadata in `copyFileCompiler` (contribution by frederik-h)

## Hakyll 4.9.5.1

- Bump blaze-html dependency to 0.9
- Bump blaze-markup dependency to 0.8
- Bump process dependency to 1.5

## Hakyll 4.9.5.0

- Fix compilation issue with `Hakyll.Check` if `checkExternal` is disabled
  (Fix by Magnus Therning)

## Hakyll 4.9.4.0

- Make `./site check` concurrent
- Bump directory dependency to 1.3
- Bump time dependency to 1.7
- Bump vector dependency to 0.12

## Hakyll 4.9.3.0

- Add a `Hakyll.Web.Redirect` module (contribution by gwern)
- Expose `Hakyll.Commands`
- Fix the exit code behaviour of `./site check`

## Hakyll 4.9.2.0

- Fix integer fields in YAML metadata (Fix by Nikolaos S. Papaspyrou)
- Bump pandoc dependency to 1.19

## Hakyll 4.9.1.0

- Allow optparse-applicative 0.13, QuickCheck 2.9, and pandoc 1.18
  (contributions by Chris Wong and Felix Yan)
- Fix extra test files for packaging source files (contribution by Julien
  Langlois)

## Hakyll 4.9.0.0

This release switches over some dependencies to alternatives, in order to clean
up some stuff and build on a wider variety of setups (stack/cabal).

- Move from `test-framework` to `tasty`
- Fix feed generator when item contains CDATA (contribution by Yann Esposito)
- Fix CompressCSS to not modify string constants (contribution by Nicole Rauch)
- Fix YAML dependency issue (contribution by Jens Peterson)
- Move from `cmdargs` to `optparse-applicative` (contribution by sk3r)
- Allow for trimming whitespace in templates (contribution by Sam Davis)
- Improve error messages for template parsing (contribution by Lorenzo
  Tabacchini)
- Improvements to the installation instructions (contribution by Thomas Koch)
- Move from `snap` to `warp` for preview server (contribution by Arguggi)
- Fix error in CompressCSS (contribution by Luca Molteni)
- Move example from XHTML to HTML5 (contribution by Peter Doherty)
- Make errors in check less verbose (contribution by Jan Tojnar)
- Work on building with GHC 8.0.1 (contribution by Rohan Jain)

## Hakyll 4.8.3.2

This release is compatible with GHC 8.0.1, although `previewServer` might not
work yet on some setups.

- Allow data-default 0.7, pandoc-citeproc 0.10, and tagsoup 0.14 (contributions
  by Paul van der Walt and Felix Yan)
- Allow binary 0.8, process 1.4, time 1.6 (contribution by Sergei Trofimovich)
- Fix issue with `.metadata` file reading

## Hakyll 4.8.3.1

- Bump scientific dependency to 0.3.4

## Hakyll 4.8.3.0

- Fix another compilation issue wrt. orphan `Show` instance from regex-tdfa
  (contribution by Sergei Trofimovich)

## Hakyll 4.8.2.0

- Fix compilation issue wrt. orphan `Show` instance from regex-tdfa

## Hakyll 4.8.1.0

- Fix compilation on windows

## Hakyll 4.8.0.1

- Fix issue with test suite

## Hakyll 4.8.0.0

- Support full YAML in page metadata
- Bump data-default dependency to 0.6
- Add snippet field for literal includes in templates (contribution by Nicolas
  Mattia)

## Hakyll 4.7.5.2

- Bump pandoc dependency to 1.17 (contribution by Felix Yan)
- Fix `unixFilter` documentation (contribution by Richard Cook)
- Bump example posts (contribution by Andrew Barchuk)
- Add a template compiler that only uses the template body (contribution by
  Bergi)

## Hakyll 4.7.5.1

- Bump pandoc and pandoc-citeproc dependencies to 1.16 and 0.9 respectively

## Hakyll 4.7.5.0

- Expose templating engine
- Fix bug in feed context precedence (contribution by Yuriy Syrovetskiy)
- Bump http-types dependency to 0.9

## Hakyll 4.7.4.0

- Expose `getItemModificationTime`

## Hakyll 4.7.3.1

- Bump pandoc-citeproc dependency to 0.8

## Hakyll 4.7.3.0

- Bump HUnit dependency to 1.3
- Add `poster` as an URL attribute (contribution by vtduncan)
- Prevent `hakyll-init` from generating directories with leading hyphen
  (contribution by Javran Cheng)

## Hakyll 4.7.2.3

- Fix time dependency in tests

## Hakyll 4.7.2.2

- Relax time dependency

## Hakyll 4.7.2.1

- Bump fsnotify dependency to 0.2

## Hakyll 4.7.2.0

- Improve documentation of `getResourceXXX` functions (contribution by Matthias
  C. M. Troffaes)
- Allow for empty templates
- Bump pandoc dependency to 1.15

## Hakyll 4.7.1.0

- Drop old-time, old-locale, time-locale-compat dependencies
- Add convenicence `pandocBiblioCompiler` (contribution by
  Matthias C. M. Troffaes)
- Add support for mediawiki (contribution by Chen Lei)

## Hakyll 4.7.0.0

- Bump pandoc to 1.14. This will break a lot of sites: since the pandoc parser
  might now return an error message, it is ran inside the `Compiler` monad where
  we can nicely handle the error.

## Hakyll 4.6.9.0

- Let caller decide exit (fix by Erik Dominikus)
- Bump pandoc-citeproc dependency

## Hakyll 4.6.8.1

- Fix test suite dependencies

## Hakyll 4.6.8.0

- Fix building on GHC 7.10 (fix by Charles Strahan)
- Add support for a custom teaser separator (contribution by Tom Sydney
  Kerckhove)
- Let Pandoc handle DocBook files (contribution by Joshua SImmons)

## Hakyll 4.6.7.1

- Bump dependencies

## Hakyll 4.6.7.0

- Bump dependencies
- Fix bug where hakyll-init would create a file called `name.cabal.cabal` (fix
  by Hans-Peter Deifel)

## Hakyll 4.6.6.0

- Fix compilation error when preview server is disabled (fix by Magnus Therning)
- Add author name by default to RSS feeds (contribution by Calen Pennington)

## Hakyll 4.6.5.0

- Bump dependencies
- Fix garbled "Listening on 0.0.0.0:8000" message
- Add `boolField` (contribution by Ferenc Wágner)

## Hakyll 4.6.4.0

- Fix another dependency handling bug when using snapshots
- Add `matchMetadata` for examining metadata when defining rules

## Hakyll 4.6.3.0

- Fix dependency handling bug

## Hakyll 4.6.2.0

- Loosen `binary` dependency
- Make dependency handling more granular so you can depend on specific snapshots
  of an item

## Hakyll 4.6.1.0

- Bump `fsnotify` and `pandoc-citeproc` dependencies
- Rewrite polling code a bit

## Hakyll 4.6.0.0

- Added `listFieldWith` function
- Improved `rulesExtraDependencies` behaviour
- Changed function syntax in templates from `$foo arg1 arg2$` to
  `$foo("arg1", "arg2")$`
- Support parsing date from directory names in addition to file names

## Hakyll 4.5.5.0

- Fix Binary instances for `pandoc` and `pandoc-citeproc`
- Fix `network-uri` dependency issue

## Hakyll 4.5.4.0

- Fix issue with HTML entities when running `withUrls` and `demoteHeaders`.
- Generate a cabal file for the initialised site.
- Add pagination support.

## Hakyll 4.5.3.0

- Bump Pandoc to 1.12.4 to include the org-mode reader.

## Hakyll 4.5.2.0

- Fix rebuilding everything issue with latest directory (contribution by Jorge
  Israel Peña)
- Fix issue with `toSiteRoot` (contribution by Izzy Cecil)
- Fix issue with tag dependencies, slightly improve caching

## Hakyll 4.5.0.0

- Fix issue with syntax highlighting and line numbers (contribution by Adelbert
  Chang)
- Improve documentation for `Context` (contribution by Daniil Frumin)
- Added `IsString` instance for `Template`
- Added the `pandocCompilerWithTransformM` function (contribution by Daniil
  Frumin)
- Make `./site check` return the right exit code (contribution by Andres Loeh)
- Use OS threads to make `./site watch` work nicely on Windows (contribution by
  Simonas Kazlauskas)
- Make the `unixFilter` function work better on windows by calling `shell`
  (contribution by Collin J. Doering)
- Add a command-line flag to bind on a user-specified host (contribution by
  chrisdotcode)

## Hakyll 4.4.3.0

- Fix issue when using `metadataRoute` after other custom routes

## Hakyll 4.4.2.0

- Fix issue where Hakyll would not detect a change if a `.metadata` file was
  deleted

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
