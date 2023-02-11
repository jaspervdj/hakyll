{- |

Twitter Card metadata, as described at
<https://developer.twitter.com/en/docs/twitter-for-websites/cards/guides/getting-started>.
This feature should be used alongside "Hakyll.Web.Meta.OpenGraph".
The following properties are supported:

+-------------------+----------------------------------------------------+
| @twitter:card@    | __Hardcoded__ card type = @"summary"@.             |
+-------------------+----------------------------------------------------+
| @twitter:creator@ | __Optional__ author's Twitter user name.           |
|                   | Taken from @twitter-creator@ context field, if set.|
+-------------------+----------------------------------------------------+
| @twitter:site@    | __Optional__ publication's Twitter user name.      |
|                   | Taken from @twitter-site@ context field, if set.   |
+-------------------+----------------------------------------------------+

To use, add 'openGraphField' and 'twitterCardField' to the template context:

@
let
  context = 'defaultContext' <> …
  postContext =
    context
    <> 'openGraphField' "opengraph" context
    <> 'twitterCardField' "twitter" context
@

and update the template:

@
\<head>
  \<title>$title$\</title>
  \<link rel="stylesheet" type="text\/css" href="\/css\/default.css" />
  $if(opengraph)$$opengraph$$endif$
  $if(twitter)$$twitter$$endif$
\</head>
@

-}
module Hakyll.Web.Meta.TwitterCard
  ( twitterCardField
  ) where

import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Hakyll.Web.Template
import Hakyll.Web.Template.Context

twitterCardField :: String -> Context String -> Context String
twitterCardField k ctx = functionField k $ \_args i -> do
  template <- twitterCardTemplate
  itemBody <$> applyTemplate template ctx i

twitterCardTemplate :: Compiler Template
twitterCardTemplate = do
  makeItem twitterCardTemplateString >>= compileTemplateItem

twitterCardTemplateString :: String
twitterCardTemplateString =
  "<meta name=\"twitter:card\" content=\"summary\" />\
  \$if(twitter-creator)$<meta property=\"twitter:creator\" content=\"$twitter-creator$\" />$endif$\
  \$if(twitter-site)$<meta property=\"twitter:site\" content=\"$twitter-site$\" />$endif$"
