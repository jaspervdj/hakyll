---
title: How to write templates
what: more information on template writing
---

## Simple templates

Simple templates are simply HTML files, with `$identifiers`. An example:

~~~~~{.html}
<html>
    <head>
        <title>$title</title>
    </head>
    <body>
        $body
    </body>
</html>
~~~~~

## Markup in templates

Most of the examples in these tutorials use HTML for templates. However, since
Hakyll 2.2, it is possible use other markup languages in your templates. Simply
use an appropriate extension, and Hakyll will pick it up. For example, you could
write your `templates/post.markdown` template as:

    # $title

    _On $date_

    $body

__Warning__: you shouldn't use markdown for your "root" template, as these
templates will never insert things like the doctype for you -- so you always
need at least one top-level HTML template.

## Hamlet templates

From Hakyll 2.3 onwards, it is possible to use [hamlet] templates. You can find
more information about hamlet on that website. Usage is fairly simple -- since
pages are strictly key-value mappings, only `$variable$` control is supported in
hamlet templates. As an example, here is the template that can be used for the
brochure site, but in hamlet:

    !!!
    %html
        %head
            %title MyAweSomeCompany - $$title$
        %body
            %h1 MyAweSomeCompany - $$title$
            #navigation
                %a!href="$$root$/index.html" Home
                %a!href="$$root$/about.html" About
                %a!href="$$root$/code.html" Code
            $body$

Hakyll will recognise hamlet templates automatically by the `.hamlet` extension.

[hamlet]: http://docs.yesodweb.com/hamlet/
