---
title: The basics
author: Jasper Van der Jeugt
---

Building and cleaning
---------------------

If you followed along with the previous tutorial, you should now have the
example site up and running. By running `./site build`, you created two
directories:

- `_site`, with your site as HTML files, ready to be deployed;
- `_cache`, which Hakyll uses internally.

`./site clean` removes these directories, and `./site rebuild` performs a
`clean` and then a `build`.

In general, it's only necessary to use `rebuild` when you made changes to your
`site.hs`, and not when you just made changes to the contents of your website.

At this point, feel free to change some files, `./site build` and see what
happens!

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
