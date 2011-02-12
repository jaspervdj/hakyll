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
module Hakyll.Web.RelativizeUrls
    ( relativizeUrls
    ) where

import Data.List (isPrefixOf)
import qualified Data.Set as S

import Text.HTML.TagSoup

-- | Relativize URL's in HTML
--
relativizeUrls :: String  -- ^ Path to the site root
               -> String  -- ^ HTML to relativize
               -> String  -- ^ Resulting HTML
relativizeUrls root = renderTags . map relativizeUrls' . parseTags
  where
    relativizeUrls' (TagOpen s a) = TagOpen s $ map (relativizeUrlsAttrs root) a
    relativizeUrls' x = x

-- | Relativize URL's in attributes
--
relativizeUrlsAttrs :: String            -- ^ Path to the site root
                    -> Attribute String  -- ^ Attribute to relativize
                    -> Attribute String  -- ^ Resulting attribute
relativizeUrlsAttrs root (key, value)
    | key `S.member` urls && "/" `isPrefixOf` value = (key, root ++ value)
    | otherwise = (key, value)
  where
    urls = S.fromList ["src", "href"]
