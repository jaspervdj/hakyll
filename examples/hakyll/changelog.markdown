---
title: Changelog
---

## Hakyll 2.2.1

- Allow custom time locale for `renderDate`.
- Render RSS feeds with `CDATA` sections.

## Hakyll 2.2

- Allow markup languages in templates.

## Hakyll 2.1.1

- Fix issues in autocompilation/preview mode.

## Hakyll 2.1

May 21, 2010

- Expose pandoc options to HakyllConfiguration.
- Allow dashes in pages.
- Some typo's and bugs fixed.

## Hakyll 2.0

March 31, 2010

- Rewrite of the API to a clean, Arrow based API.
- Added built-in support for RSS and Atom.
- Added more documentation.
- Added pagination.
- Many bugfixes.

## Hakyll 1.4

February 17, 2010

- Added an autocompilation feature.
- Support for index URL's (`enableIndexUrl`).

## Hakyll 1.3

January 30, 2010

- Added categories in addition to tags.
- Added `createListing` and `createListingWith` function for a more high-level
  way to create listings.

## Hakyll 1.2

January 27, 2010

- `Data.Binary` is now used for serialization.
- Rewrite of the caching system.
- Specialized data structure for templates.
- Caching of pages and templates.

## Hakyll 1.1

January 19, 2010

- Switched to a custom `Hakyll` monad stack instead of the `IO` monad.
- Page sections.
- Combining renderables.
- `renderAndConcat` can now use multiple templates.

## Hakyll 1.0

January 14, 2009

- First stable release.
- Custom templating system.
- Added `$root` key for relative URL's.

## Hakyll 0.4

January 8, 2010

- Added examples.
- Added `ContextManipulation` type.

## Hakyll 0.3

December 28, 2009

- Added a general `directory` function.
- Added CSS compression.
- Added tag support.
- Added a simple HTTP server for testing purposes.

## Hakyll 0.2

December 16, 2010

- Abstracted `Renderable` type.
- Added simple caching and dependency checking.

## Hakyll 0.1

December 5, 2009

- Initial release.
