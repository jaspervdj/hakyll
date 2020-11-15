---
title: Using Hakyll with GitHub Pages
author: Erik Stevenson
type: article
---

## Introduction

[GitHub Pages](http://pages.github.com) has become a popular static website hosting solution due to its simplicity. Simply push a couple files to a repository and it's off to the races.

Working with Hakyll on a GitHub Pages-hosted website is complicated slightly due to Hakyll outputting files to a ```_site``` subdirectory, our desire to have the source code as well as the compiled site stored in a single repository, and our desire to automate it.

This guide will walkthrough the creation and setup of a GitHub site that has two independent branches.

1. ```master``` - This is where your site lives. It's what you see when you go to ```https://<your repo>.github.io```. This branch *needs* to be called master.
2. ```develop``` - This is where your website's source is. That's all your Haskell code, your posts and templates, etc, and it's where you do work from. This name was chosen arbitrarily and can be freely substituted for any name of your choosing.

When you're finished, you will be able to, with one command, refresh your website's contents and send any changes to your GitHub Page.

In the interest of keeping this guide as simple as possible, I'll be making a few assumptions.

1. Haskell is being used with [Stack](http://docs.haskellstack.org/en/stable/README/).
2. Creating a user/organization site (vice a project site).
3. You haven't changed Hakyll's default output directory of '_site/'.

These instructions should be easy to adapt for any situation though.

## GitHub Setup

1. If required, create a new repository for your blog.
2. If required, create a ```master``` branch.
2. If applicable/desired, create/add to your repository any files that your site needs that will not be produced by your Hakyll project. For example, ```CNAME``` as outlined [here](https://help.github.com/articles/setting-up-your-pages-site-repository/).
3. Create a ```.gitignore``` file with at a minimum, the following entries:

```
_cache/
_site/
.stack-work/
```

## Local setup

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

## Deployment

So everything's all setup and we're ready to deploy.

> **Note:** Performing the following commands from your ```develop``` branch is recommended since you will end up back in that branch at the end.

Temporarily save any uncommitted changes that may exist in the current branch.

```
git stash
```

Ensure we are in the correct branch.

```
git checkout develop
```

We need to be able to run the executable that generates the website, so we need
to compile it first.  If you are using `stack`, this can be done using:

```
stack build
```

Now get a clean build of our site.

```
stack exec myblog clean
stack exec myblog build
```

Update the local list of remote branches to ensure we're able to checkout the branch we want in the next step.

```
git fetch --all
```

### Deployment Using ```git-commit-filetree```

The [`git-commit-filetree`](https://github.com/cynic-net/git-commit-filetree) command automates the process of committing the files for your compiled site to another branch in the current repo. This replaces all steps after the post-build branch update (```git fetch --all``` above).

You can use the [```bin/git-commit-filetree```](https://github.com/cynic-net/git-commit-filetree/blob/master/bin/git-commit-filetree) script by putting it in any directory in your ```$PATH``` (e.g., ```~/.local/bin```) or by specifying the full path to it when you run it. Here we suggest simply copying that file to the root of the ```develop``` branch in your repo and commiting it so that it's always available to anybody using the repo.

If you do not already have a local ```master``` branch in your repo, you will need to create one. This needs to be done only once in a repo. (If you have not done this, you will later see a message ```Invalid ref: refs/heads/master``` from ```git-commit-filetree```.)

```
git branch master --track origin/master
```

Commit the files you built under ```_site``` to the root of the file tree of the  ```master``` branch and push that branch up to the server. This will entirely replace the file tree, so you need not worry about removing old files manually.

```
./git-commit-filetree master _site/
git push origin master:master
```

### Manual Deployment

Switch to the `master` branch. 

> **Note:** Checking out this branch does not overwrite the files that Hakyll just produced because we have '_site' listed in both .gitignore files.

```
git checkout -b master --track origin/master
```

Next, copy the freshly made contents of '_site' over the old ones.

> **Note:** Deleting a file from your site's source will not remove it from your `master` repository if it has already been published. An alternative to `cp` is discussed at the end of this guide.

```
cp -a _site/. .
```

Commit our changes.

```
git add -A
git commit -m "Publish."
```

And send them to GitHub.

```
git push origin master:master
```

Final clean up and return to the original state.

```
git checkout develop
git branch -D master
git stash pop
```

## Putting it all together

Below is a complete listing of all the commands used to automate deployment to Github Pages. A `deployCommand` can be set as part of Hakyll's configuration options. More information and an example is provided [here](https://jaspervdj.be/hakyll/reference/Hakyll-Core-Configuration.html).

```
# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout develop

# Build new files
stack exec myblog clean
stack exec myblog build

# Get previous files
git fetch --all
```

### Deployment Using ```git-commit-filetree```

```
#   The following command is needed only if you have not already created
#   a local `master` branch. (Running it more than once is harmless.)
git branch master --track origin/master

./git-commit-filetree master _site/
git push origin master:master
```

### Manual Deployment

```
git checkout -b master --track origin/master

# Overwrite existing files with new files
cp -a _site/. .

# Commit
git add -A
git commit -m "Publish."

# Push
git push origin master:master

# Restoration
git checkout develop
git branch -D master
git stash pop
```

*And that's it.*

## Removing old files with `rsync`

Earlier it was mentioned a flaw is that deleted files will persist in the published site until deleted manually. This is easily overcome by using `rsync` instead of `cp`.

```
rsync -a --filter='P _site/'      \
         --filter='P _cache/'     \
         --filter='P .git/'       \
         --filter='P .gitignore'  \
         --filter='P .stack-work' \
         --delete-excluded        \
         _site/ .
```

The only drawback this approach has is the requirement that *every* file in your site "go through" Hakyll. Fortunately, in many cases this is not an issue.
