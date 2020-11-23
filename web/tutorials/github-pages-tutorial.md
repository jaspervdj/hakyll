---
title: Using Hakyll with GitHub Pages
author: Erik Stevenson
type: article
---

## Introduction

[GitHub Pages](http://pages.github.com) has become a popular static website hosting solution due to its simplicity. Simply push a couple files to a repository and it's off to the races.

Working with Hakyll on a GitHub Pages-hosted website is complicated slightly due to Hakyll outputting files to a ```_site``` subdirectory, our desire to have the source code as well as the compiled site stored in a single repository, and our desire to automate it.

This guide will walkthrough the creation and setup of a GitHub site that works on a single `master` branch.

When you're finished, you will be able to, with one command, refresh your website's contents and send any changes to your GitHub Page.

In the interest of keeping this guide as simple as possible, I'll be making a few assumptions.

1. Haskell is being used with [Stack](http://docs.haskellstack.org/en/stable/README/).
2. Creating a user/organization site (vice a project site).
3. You haven't changed Hakyll's default output directory of '_site/'.

These instructions should be easy to adapt for any situation though.

## GitHub Setup

1. If required, create a new GitHub repository for your blog.
2. If required, create a `master` branch.
3. in the Settings of your GitHub project define that the `/docs` folder from the `master` branch should be used as document-root of your site.
   Please refer to the [documentation](https://docs.github.com/en/free-pro-team@latest/github/working-with-github-pages/configuring-a-publishing-source-for-your-github-pages-site#choosing-a-publishing-source)
   in case of problems.
4. Create a .gitignore file with at a minimum, the following entries:

```
_cache/
.stack-work/
```

## Local setup

1. If required, create a new Hakyll project. If you're a stack user, there is a Hakyll template available that makes this step easy.

```stack new myblog hakyll-template```

2. Create a ```.gitignore``` file in your blog's directory with at a minimum, the same directories listed as in the GitHub repository.
3. Use the following git commands to setup your local repository.

```bash
git init
# track all the source files for our blog.
git add .
# make our first commit
git commit -m "initial commit."
# and add the GitHub repository as a remote.
git remote add origin <URL to your GitHub pages repository>
```

### Modify site.hs

In order to make Hakyll generate the site into the `/docs` folder you'll have to edit the Hakyll Main module (`site.hs` if you use the stack template):

```haskell
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = do
  hakyllWith config $ do
  ...
```

## Deployment

So everything’s all setup, and we’re ready to deploy.

We need to be able to run the executable that generates the website, so we need to compile it first. If you are using stack, this can be done using:

```bash
stack build
```

Next we get a clean build of our site:

```bash
stack exec myblog clean
stack exec myblog build
```

After this step you should see a folder `docs` under your projects root folder, which contains the generated Hakyll site.

Now we commit our changes:

```bash
git add -A
git commit -m "Publish."
```

And send them to GitHub:

```bash
git push origin master:master
```

That's all.

Within a few seconds your Hakyll site should be visible under your GitHub Pages URL!

## Putting it all together

Below is a complete listing of all the commands used to automate deployment to Github Pages. A `deployCommand` can be set as part of Hakyll's configuration options. More information and an example is provided [here](https://jaspervdj.be/hakyll/reference/Hakyll-Core-Configuration.html).

```
# Verify correct branch
git checkout master

# Build new files
stack exec myblog clean
stack exec myblog build

# Commit
git add -A
git commit -m "Publish."

# Push
git push origin master:master
```

*And that's it.*