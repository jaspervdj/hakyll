---
title: Hakyll 3 to Hakyll 4 migration guide
author: Jasper Van der Jeugt
---

Introduction
------------

This tutorial gives a quick overview on how you can port your blog/website from
Hakyll 3.X to Hakyll 4. A lot of changes have happened, so it might be useful to
read through the [tutorial series](/tutorials.html) before porting your website.

Arrow becomes Monad
-------------------

In Hakyll 3.X, `Compiler` was an instance of `Arrow`. Since Hakyll 4, `Compiler`
is a `Monad`. This means that previous chains such as:

```haskell
compile $ someCompiler
    >>> someOtherCompiler
    >>> anotherCompiler
```

Now take the general form of:

```haskell
compile $ someCompiler
    >>= someOtherCompiler
    >>= anotherCompiler
```

Page goes away
--------------

The `Page` type in Hakyll 3.X has been removed and replaced by an `Item` type.
`pageCompiler` no longer exists -- where you previously used this, you probably
want to use `pandocCompiler` instead.

`Page`s were manipulated using `setField`/`getField` functions in Hakyll 3.X.
In Hakyll 4, all metadata is completely immutable, so these functions have been
removed. In order to format and add fields, use a `Context` -- see the next
section.

Template changes
----------------

The template format has become slightly more flexible, whereas in Hakyll 3.X
only keys such as this were allowed:

```html
<h1>$title$</h1>
```

we now allow arbitrary strings. This will be really useful in the future.

```html
<h1>$uppercase title$</h1>
```

Some template functions have been renamed:

- `applyTemplateCompiler` becomes: `loadAndApplyTemplate`
- `applySelf` becomes: `applyAsTemplate`

Instead of setting fields in a `Page` before applying a template, we now use a
`Context`. More information on context can be found in
[this tutorial](/tutorials/04-compilers.html). For migration, you basically want
to map every `setField` to a field in a `Context`.

Metacompilers go away
---------------------

For tags, the [Hakyll.Web.Tags] module still provides a solution. In other
cases, the `preprocess` function should be able to compensate for this.

[Hakyll.Web.Tags]: /reference/Hakyll-Web-Tags.html
