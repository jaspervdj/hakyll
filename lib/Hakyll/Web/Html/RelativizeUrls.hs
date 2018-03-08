--------------------------------------------------------------------------------
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
module Hakyll.Web.Html.RelativizeUrls
    ( relativizeUrls
    , relativizeUrlsWith
    ) where


--------------------------------------------------------------------------------
import           Data.List            (isPrefixOf)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Web.Html


--------------------------------------------------------------------------------
-- | Compiler form of 'relativizeUrls' which automatically picks the right root
-- path
relativizeUrls :: Item String -> Compiler (Item String)
relativizeUrls item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r  -> fmap (relativizeUrlsWith $ toSiteRoot r) item


--------------------------------------------------------------------------------
-- | Relativize URL's in HTML
relativizeUrlsWith :: String  -- ^ Path to the site root
                   -> String  -- ^ HTML to relativize
                   -> String  -- ^ Resulting HTML
relativizeUrlsWith root = withUrls rel
  where
    isRel x = "/" `isPrefixOf` x && not ("//" `isPrefixOf` x)
    rel x   = if isRel x then root ++ x else x
