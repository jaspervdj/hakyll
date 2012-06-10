-- | Provides utilities to manipulate URL's
--
module Hakyll.Web.Urls
    ( withUrls
    , toUrl
    , toSiteRoot
    , isExternal
    ) where

import Data.List (isPrefixOf)
import Data.Char (toLower)
import System.FilePath (splitPath, takeDirectory, joinPath)
import qualified Data.Set as S

import qualified Text.HTML.TagSoup as TS

-- | Apply a function to each URL on a webpage
--
withUrls :: (String -> String) -> String -> String
withUrls f = renderTags' . map tag . TS.parseTags
  where
    tag (TS.TagOpen s a) = TS.TagOpen s $ map attr a
    tag x                = x
    attr (k, v)          = (k, if k `S.member` refs then f v else v)
    refs                 = S.fromList ["src", "href"]

-- | Customized TagSoup renderer. (The default TagSoup renderer escape CSS
-- within style tags.)
--
renderTags' :: [TS.Tag String] -> String
renderTags' = TS.renderTagsOptions TS.renderOptions
    { TS.optRawTag = (`elem` ["script", "style"]) . map toLower
    }

-- | Convert a filepath to an URL starting from the site root
--
-- Example:
--
-- > toUrl "foo/bar.html"
--
-- Result:
--
-- > "/foo/bar.html"
--
toUrl :: FilePath -> String
toUrl ('/' : xs) = '/' : xs
toUrl url        = '/' : url

-- | Get the relative url to the site root, for a given (absolute) url
--
toSiteRoot :: String -> String
toSiteRoot = emptyException . joinPath . map parent
           . filter relevant . splitPath . takeDirectory
  where
    parent            = const ".."
    emptyException [] = "."
    emptyException x  = x
    relevant "."      = False
    relevant "/"      = False
    relevant _        = True

-- | Check if an URL links to an external HTTP(S) source
--
isExternal :: String -> Bool
isExternal url = any (flip isPrefixOf url) ["http://", "https://"]
