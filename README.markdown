# hakyll

![CI](https://github.com/jaspervdj/hakyll/workflows/CI/badge.svg)

Hakyll is a static site generator library in Haskell. More information
(including a tutorial) can be found on
[the hakyll homepage](http://jaspervdj.be/hakyll).

You can install this library using cabal:

    cabal install hakyll

Or using stack:

    stack install hakyll

When using _hakyll_ for a single website, or for the sake of trying out _hakyll_, users might find it more convenient to install it in a local repository (i.e. the repository from which they will build and deploy the website). In that case simply create a template directory using your favorite project manager and run the above command from it:

    mkdir <project's directory>
    cd <project's directory>
    cabal init
    
Or for stack users replace the last command with:

    stack init