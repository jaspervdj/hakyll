# Releasing a new version of hakyll

1. Update the version in `hakyll.cabal` and update the `CHANGELOG.md` file to describe changes associated with this version;
2. Commit changes to `hakyll.cabal` and `CHANGELOG.md`;
3. Tag the commit and push to GitHub:
    
    ```bash
    $ git tag -a "vW.X.Y.Z" && git push origin "vW.X.Y.Z"
    ```

4. Create a source distribution:

    ```bash
    $ cabal sdist
    Wrote tarball sdist to
    (...)/dist-newstyle/sdist/hakyll-W.X.Y.Z.tar.gz
    ```

5. Upload to Hackage:

    ```bash
    $ cabal upload --publish (...)/dist-newstyle/sdist/hakyll-W.X.Y.Z.tar.gz
    ```

That's it!