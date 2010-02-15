---
title: Tips and Tricks
what: some various tips and tricks about Hakyll
---

## Auto-compilation

Hakyll features a simple _auto-compilation_ mode. This is invoked by running

~~~~~
[jasper@alice ~]$ ./hakyll preview
Starting hakyll server on port 8000...
~~~~~

Now, Hakyll will recompile your site when you change files, so you can just
refresh in your browser. There is one more thing to note: this will not update
your site automatically when `hakyll.hs` changes. So if you make any changes to
the configuration file, you'll have to compile it again, and then you can enter
`preview` mode again.
