---
title: Changelog
---

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

January 14, 2010

- First stable release.
