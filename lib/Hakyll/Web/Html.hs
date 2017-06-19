--------------------------------------------------------------------------------
-- | Provides utilities to manipulate HTML pages
module Hakyll.Web.Html
    ( -- * Generic
      withTags

      -- * Headers
    , demoteHeaders

      -- * Url manipulation
    , getUrls
    , withUrls
    , toUrl
    , toSiteRoot
    , isExternal

      -- * Stripping/escaping
    , stripTags
    , escapeHtml
    ) where


--------------------------------------------------------------------------------
import           Data.Char                       (digitToInt, intToDigit,
                                                  isDigit, toLower)
import           Data.List                       (isPrefixOf)
import qualified Data.Set                        as S
import           System.FilePath.Posix           (joinPath, splitPath,
                                                  takeDirectory)
import           Text.Blaze.Html                 (toHtml)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.HTML.TagSoup               as TS
import           Network.URI                     (isUnreserved, escapeURIString)


--------------------------------------------------------------------------------
-- | Map over all tags in the document
withTags :: (TS.Tag String -> TS.Tag String) -> String -> String
withTags f = renderTags' . map f . parseTags'


--------------------------------------------------------------------------------
-- | Map every @h1@ to an @h2@, @h2@ to @h3@, etc.
demoteHeaders :: String -> String
demoteHeaders = withTags $ \tag -> case tag of
    TS.TagOpen t a -> TS.TagOpen (demote t) a
    TS.TagClose t  -> TS.TagClose (demote t)
    t              -> t
  where
    demote t@['h', n]
        | isDigit n = ['h', intToDigit (min 6 $ digitToInt n + 1)]
        | otherwise = t
    demote t        = t


--------------------------------------------------------------------------------
isUrlAttribute :: String -> Bool
isUrlAttribute = (`elem` ["src", "href", "data", "poster"])


--------------------------------------------------------------------------------
getUrls :: [TS.Tag String] -> [String]
getUrls tags = [v | TS.TagOpen _ as <- tags, (k, v) <- as, isUrlAttribute k]


--------------------------------------------------------------------------------
-- | Apply a function to each URL on a webpage
withUrls :: (String -> String) -> String -> String
withUrls f = withTags tag
  where
    tag (TS.TagOpen s a) = TS.TagOpen s $ map attr a
    tag x                = x
    attr (k, v)          = (k, if isUrlAttribute k then f v else v)


--------------------------------------------------------------------------------
-- | Customized TagSoup renderer. The default TagSoup renderer escape CSS
-- within style tags, and doesn't properly minimize.
renderTags' :: [TS.Tag String] -> String
renderTags' = TS.renderTagsOptions TS.RenderOptions
    { TS.optRawTag   = (`elem` ["script", "style"]) . map toLower
    , TS.optMinimize = (`S.member` minimize) . map toLower
    , TS.optEscape   = id
    }
  where
    -- A list of elements which must be minimized
    minimize = S.fromList
        [ "area", "br", "col", "embed", "hr", "img", "input", "meta", "link"
        , "param"
        ]


--------------------------------------------------------------------------------
-- | Customized TagSoup parser: do not decode any entities.
parseTags' :: String -> [TS.Tag String]
parseTags' = TS.parseTagsOptions (TS.parseOptions :: TS.ParseOptions String)
    { TS.optEntityData   = \(str, b) -> [TS.TagText $ "&" ++ str ++ [';' | b]]
    , TS.optEntityAttrib = \(str, b) -> ("&" ++ str ++ [';' | b], [])
    }


--------------------------------------------------------------------------------
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
-- This also sanitizes the URL, e.g. converting spaces into '%20'
toUrl :: FilePath -> String
toUrl url = case url of
    ('/' : xs) -> '/' : sanitize xs
    xs         -> '/' : sanitize xs
  where
    -- Everything but unreserved characters should be escaped as we are
    -- sanitising the path therefore reserved characters which have a
    -- meaning in URI does not appear. Special casing for `/`, because it has
    -- a special meaning in FilePath as well as in URI.
    sanitize = escapeURIString (\c -> c == '/' || isUnreserved c)


--------------------------------------------------------------------------------
-- | Get the relative url to the site root, for a given (absolute) url
toSiteRoot :: String -> String
toSiteRoot = emptyException . joinPath . map parent
           . filter relevant . splitPath . takeDirectory
  where
    parent            = const ".."
    emptyException [] = "."
    emptyException x  = x
    relevant "."      = False
    relevant "/"      = False
    relevant "./"     = False
    relevant _        = True


--------------------------------------------------------------------------------
-- | Check if an URL links to an external HTTP(S) source
isExternal :: String -> Bool
isExternal url = any (flip isPrefixOf url) ["http://", "https://", "//"]


--------------------------------------------------------------------------------
-- | Strip all HTML tags from a string
--
-- Example:
--
-- > stripTags "<p>foo</p>"
--
-- Result:
--
-- > "foo"
--
-- This also works for incomplete tags
--
-- Example:
--
-- > stripTags "<p>foo</p"
--
-- Result:
--
-- > "foo"
stripTags :: String -> String
stripTags []         = []
stripTags ('<' : xs) = stripTags $ drop 1 $ dropWhile (/= '>') xs
stripTags (x : xs)   = x : stripTags xs


--------------------------------------------------------------------------------
-- | HTML-escape a string
--
-- Example:
--
-- > escapeHtml "Me & Dean"
--
-- Result:
--
-- > "Me &amp; Dean"
escapeHtml :: String -> String
escapeHtml = renderHtml . toHtml
