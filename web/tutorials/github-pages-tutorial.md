## Using Hakyll with GitHub Pages

In the interest of keeping this guide as simple as possible, I'll be making a few assumptions.

1. Haskell is being used with [Stack](http://docs.haskellstack.org/en/stable/README/).
2. Creating a user/organization site (vice a project site).
3. You haven't changed Hakyll's default output directory of '_site/'.

These instructions should be easy to adapt for any situation though.

### GitHub Setup

1. If required, create a new repository for your blog.
2. If required, create a ```master``` branch.
2. If applicable/desired, create/add to your repository any files that your site needs that will not be produced by your Hakyll project. For example, ```CNAME``` as outlined [here](https://help.github.com/articles/setting-up-your-pages-site-repository/).
3. Create a ```.gitignore``` file with at a minimum, the following entries:

```
_cache/
_site/
.stack-work/
```

### Local setup

1. If required, create a new Hakyll project. If you're a stack user, there is a Hakyll template available that makes this step easy.

```stack new myblog hakyll-template```

2. Create a ```.gitignore``` file in your blog's directory with at a minimum, the same directories listed as in the GitHub repository.
3. Use the following git commands to setup your local repository.

```
git init
# create new branch called develop and switch to it.
git checkout -b develop
# track all the source files for our blog.
git add .
# make our first commit
git commit -m "initial commit."
# and add the GitHub repository as a remote.
git remote add origin <URL to your GitHub pages repository>
```

### Deployment

So everything's all setup and we're ready to deploy.

> Note: Performing the following commands from your ```develop``` branch is recommended since you will end up back in that branch at the end.

Temporarily save any uncommitted changes that may exist in the current branch.

```git stash```

Ensure we are in the correct branch.

```git checkout develop```

Get a clean build of our site.

```
stack exec myblog clean
stack exec myblog build
```

Update the local list of remote branches to ensure we're able to checkout the branch we want in the next step.

```git fetch -all```

Switches to a new branch called "publish" that tracks the origin "master" branch. **Note:** Checking out the publish branch does not overwrite the files that Hakyll just produced because we have '_site' in both .gitignore files.

```git checkout -b publish --track origin/master```

Copy the freshly made contents of '_site' over the old ones. Note that if a file is *no longer* being produced (for example if you deleted a blog posting), it will continue to persist in your published site until it's been removed from that repository as well.

```cp -a _site/. .```

Commit our changes.

```
git add -A
git commit -m "publish."
```

And send them to GitHub.

```git push origin publish:master```

Final clean up and return to the original state.

```
git checkout develop
git branch -D publish
git stash pop
```

### *And that's it.*

Here is a full listing of the script.

```
# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout develop

# Build new files
stack exec myblog clean
stack exec myblog build

# Get previous files
git fetch -all
git checkout -b publish --track origin/master

# Overwrite existing files with new files
cp -a _site/. .

# Commit
git add -A
git commit -m "publish."

# Push
git push origin publish:master

# Restoration
git checkout develop
git branch -D publish
git stash pop
```
