-- | Miscellaneous URL manipulation functions.
--
module Hakyll.Web.Util.Url
    ( toUrl
    , toSiteRoot
    ) where

import System.FilePath (splitPath, takeDirectory, joinPath)

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
    parent = const ".."
    emptyException [] = "."
    emptyException x  = x
    relevant "." = False
    relevant "/" = False
    relevant _   = True
