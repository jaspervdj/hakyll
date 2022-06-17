# hakyll

![CI](https://github.com/jaspervdj/hakyll/workflows/CI/badge.svg)

Hakyll is a static site generator library in Haskell. More information
(including a tutorial) can be found on
[the hakyll homepage](http://jaspervdj.be/hakyll).

To install this library, first create a new directory and under it:

    cabal init

And then:
    
    <directory name>.cabal
    ----------------------
    build-depends: base ^>=4.15.1.0, hakyll

Or for `stack` users:

    stack init

And then:

    package.yaml
    ------------
    dependencies:
     - hakyll
