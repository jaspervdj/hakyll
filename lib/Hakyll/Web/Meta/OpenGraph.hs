{- |

Open Graph metadata, as described at <https://ogp.me/>.  This
implementation supports the following properties:

+------------------+----------------------------------------------------+
| @og:type@        | __Hardcoded__ value @"article"@                    |
+------------------+----------------------------------------------------+
| @og:url@         | __Required__ concatenation of @root@ and @url@     |
|                  | context fields, both of which are required.        |
+------------------+----------------------------------------------------+
| @og:title@       | __Required__ title of article, from @title@ field. |
+------------------+----------------------------------------------------+
| @og:description@ | __Optional__ brief description taken from context  |
|                  | field @og-description@, if set.                    |
+------------------+----------------------------------------------------+
| @og:image@       | __Optional__ image URL taken from context          |
|                  | field @og-image@, if set.                          |
+------------------+----------------------------------------------------+

To use, add 'openGraphField' to the template context:

@
let
  context = 'defaultContext' <> …
  postContext = context <> 'openGraphField' "opengraph" context
@

and update the template:

@
\<head>
  \<title>$title$</title>
  \<link rel="stylesheet" type="text\/css" href="\/css\/default.css" />
  $if(opengraph)$$opengraph$$endif$
\</head>
@

See also "Hakyll.Web.Meta.TwitterCard".

-}
module Hakyll.Web.Meta.OpenGraph
  ( openGraphField
  ) where

import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Hakyll.Web.Template
import Hakyll.Web.Template.Context

openGraphField :: String -> Context String -> Context String
openGraphField k ctx = functionField k $ \_args i -> do
  template <- openGraphTemplate
  itemBody <$> applyTemplate template ctx i

openGraphTemplate :: Compiler Template
openGraphTemplate = do
  makeItem openGraphTemplateString >>= compileTemplateItem

openGraphTemplateString :: String
openGraphTemplateString =
  "<meta property=\"og:type\" content=\"article\" />\
  \<meta property=\"og:url\" content=\"$root$$url$\" />\
  \<meta property=\"og:title\" content=\"$title$\" />\
  \$if(og-description)$\
  \<meta property=\"og:description\" content=\"$og-description$\" />\
  \$endif$\
  \$if(og-image)$\
  \<meta property=\"og:image\" content=\"$og-image$\" />\
  \$endif$\
  \"
