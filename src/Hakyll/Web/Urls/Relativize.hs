-- | This module exposes a function which can relativize URL's on a webpage.
--
-- This means that one can deploy the resulting site on
-- @http:\/\/example.com\/@, but also on @http:\/\/example.com\/some-folder\/@
-- without having to change anything (simply copy over the files).
--
-- To use it, you should use absolute URL's from the site root everywhere. For
-- example, use
--
-- > <img src="/images/lolcat.png" alt="Funny zomgroflcopter" />
--
-- in a blogpost. When running this through the relativize URL's module, this
-- will result in (suppose your blogpost is located at @\/posts\/foo.html@:
--
-- > <img src="../images/lolcat.png" alt="Funny zomgroflcopter" />
--
module Hakyll.Web.Urls.Relativize
    ( relativizeUrlsCompiler
    , relativizeUrls
    ) where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((&&&), (>>^))
import Data.List (isPrefixOf)

import Hakyll.Core.Compiler
import Hakyll.Web.Page
import Hakyll.Web.Urls

-- | Compiler form of 'relativizeUrls' which automatically picks the right root
-- path
--
relativizeUrlsCompiler :: Compiler (Page String) (Page String)
relativizeUrlsCompiler = getRoute &&& id >>^ uncurry relativize
  where
    relativize Nothing  = id
    relativize (Just r) = fmap (relativizeUrls $ toSiteRoot r)

-- | Relativize URL's in HTML
--
relativizeUrls :: String  -- ^ Path to the site root
               -> String  -- ^ HTML to relativize
               -> String  -- ^ Resulting HTML
relativizeUrls root = withUrls rel
  where
    isRel x = "/" `isPrefixOf` x && not ("//" `isPrefixOf` x)
    rel x = if isRel x then root ++ x else x
