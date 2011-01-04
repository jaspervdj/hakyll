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
