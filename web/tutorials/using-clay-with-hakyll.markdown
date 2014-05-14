---
title: Using Clay with Hakyll
author: Jasper Van der Jeugt
---

[Clay](http://sebastiaanvisser.github.com/clay/) is a nice CSS preprocesser
written in Haskell. There are multiple options to use this together with Hakyll,
but in this short tutorial I focus on what I think is the best way.

This method requires every Clay file to have a `main` function which just prints
the CSS. This would be an example of such a file:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import           Clay

test :: Css
test = ...

main :: IO ()
main = putCss test
```

If you prefer the compact version, you're going to have an extra import:


``` haskell
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = T.putStr $ renderWith compact test
```


Let's assume such a file is called `css/foo.hs`. In our compiled site, we want
to map this to `css/foo.css`. Hence, the route is a simple `setExtension`. For
compilation, we simply pass the Clay file through `runghc` with no options.

```haskell
match "css/*.hs" $ do
    route   $ setExtension "css"
    compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

-- cabal sandbox users can use (unixFilter "cabal" ["exec", "runghc"])
-- given that you've added cabal to your PATH
```

The major advantage of using this method (as opposed to importing the Clay files
in `site.hs`) is that now Hakyll will only recompile the Clay files when
necessary, and you don't have to manually recompile your `site.hs` each time you
want to tweak your CSS a little.
