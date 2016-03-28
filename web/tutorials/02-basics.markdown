---
title: The basics
author: Jasper Van der Jeugt
---

Building and cleaning
---------------------

If you followed along with the previous tutorial, you should now have the
example site up and running. By running `stack exec site build`, you created
two directories:

- `_site`, with your site as HTML files, ready to be deployed;
- `_cache`, which Hakyll uses internally.

`stack exec site clean` removes these directories, and `stack exec site
rebuild` performs a `clean` and then a `build`.

In general, you want to use `stack exec site build` when you just made changes
to the contents of your website. If you made changes to `site.hs`, you need to
recompile `site.hs` followed by a rebuild:

    stack build
    stack exec site rebuild

At this point, feel free to change some files, `stack exec site build` and see
what happens!

Pages and metadata
------------------

You might've noticed that the markdown pages all start with a block:

    ---
    title: Contact
    ---

    I live...

This is entirely optional, but useful for providing extra information
("metadata") about items. All items can have metadata: since it's not really
convenient to add such a header to an image, you can also do this using a
separate file.

For a file called `images/foo.png`, you can add an `images/foo.png.metadata`
file with contents:

    title: An image of a cow
