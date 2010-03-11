---
title: Arrows: A Crash Course
what: illustrates how Arrows are used in hakyll
---

## Do I really need to know this stuff?

Maybe. You don't need it when you only use the basic Hakyll functions, but
Arrows are used a lot in the Hakyll code, and a lot of the more advanced
features make use of Arrows. Besides, it's a pretty interesting subject.

## What is an "Arrow"

Arrows are comparable with monads. In fact, monads are a subset of arrows.
Arrows allow you to represent a "computation". This is all pretty vague, so
let's skip directly to the Arrows used in Hakyll.

## HakyllAction

The Arrow used throughout Hakyll is called `HakyllAction`. Arrows have two
type parameters, so it's actually `HakyllAction a b`. You can think of `a`
as the input for our action, and `b` is the corresponding output. Let's look
at the type of `createPage`:

~~~~~{.haskell}
createPage :: FilePath -> HakyllAction () Context
~~~~~

So, you give `createPage` a `FilePath`, and it creates a `HakyllAction` that
produces a `Context` out of thin air. Now, we want to render the `Context` we
just loaded with a template. The type of the `render` function is:

~~~~~{.haskell}
render :: FilePath -> HakyllAction Context Context
~~~~~

We pass the file name of a template to the `render` function, and we get a
`HakyllAction` that creates a `Context` from another `Context`. The result
of the `render` operation (so basically the rendered template) will be placed
in the `$body` field of the new `Context`. But we still haven't saved our
result, so let's do that using the `writePage` function.

~~~~~{.haskell}
writePage :: HakyllAction Context ()
~~~~~

This function writes our result and returns nothing.

## Composition

Now, let's look at the big picture.

![Arrow illustration]($root/images/arrow-composition.png)

If these were regular functions, we could've composed them using the `.`
operator. Since they're arrows, we'll have to use the `>>>` operator.

~~~~~{.haskell}
test :: HakyllAction () ()
test =   createPage "test.markdown"
     >>> render "template.html"
     >>> writePage
~~~~~

Now, we only have to execute our test.

~~~~~{.haskell}
runHakyllActionIfNeeded test
~~~~~

## Aso, the point emerges

The `runHakyllActionIfNeeded` suggests why we use arrows. `HakyllAction` is more
than just a function, it also tracks dependencies. This Hakyll to only execute
our functions when it is really needed. In this particular case, `test` would
only be executed if either `test.markdown` or `template.html` were recently
changed.

## The gist of it

- Arrows really aren't complicated.
- Compose them using `>>>`.
- `HakyllAction` tracks dependencies for you. Use it.
