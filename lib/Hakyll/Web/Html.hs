--------------------------------------------------------------------------------
-- | Provides utilities to manipulate HTML pages
module Hakyll.Web.Html
    ( -- * Generic
      withTags
    , withTagList

      -- * Headers
    , demoteHeaders
    , demoteHeadersBy

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
import           Data.Either                     (fromRight)
import           Data.List                       (isPrefixOf, intercalate)
import           Data.Maybe                      (fromMaybe)
import qualified Data.Set                        as S
import           Control.Monad                   (void)
import           System.FilePath                 (joinPath, splitPath,
                                                  takeDirectory)
import           Text.Blaze.Html                 (toHtml)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Parsec                     as P
import qualified Text.Parsec.Char                as PC
import qualified Text.HTML.TagSoup               as TS
import           Network.URI                     (isUnreserved, escapeURIString)


--------------------------------------------------------------------------------
import           Hakyll.Core.Util.String         (removeWinPathSeparator)


--------------------------------------------------------------------------------
-- | Map over all tags in the document
withTags :: (TS.Tag String -> TS.Tag String) -> String -> String
withTags = withTagList . map

-- | Map over all tags (as list) in the document
withTagList :: ([TS.Tag String] -> [TS.Tag String]) -> String -> String
withTagList f = renderTags' . f . parseTags'

--------------------------------------------------------------------------------
-- | Map every @h1@ to an @h2@, @h2@ to @h3@, etc.
demoteHeaders :: String -> String
demoteHeaders = demoteHeadersBy 1

--------------------------------------------------------------------------------
-- | Maps any @hN@ to an @hN+amount@ for any @amount > 0 && 1 <= N+amount <= 6@.
demoteHeadersBy :: Int -> String -> String
demoteHeadersBy amount
  | amount < 1 = id
  | otherwise = withTags $ \tag -> case tag of
    TS.TagOpen t a -> TS.TagOpen (demote t) a
    TS.TagClose t  -> TS.TagClose (demote t)
    t              -> t
  where
    demote t@['h', n]
        | isDigit n = ['h', intToDigit (min 6 $ digitToInt n + amount)]
        | otherwise = t
    demote t        = t


--------------------------------------------------------------------------------
isUrlAttribute :: String -> Bool
isUrlAttribute = (`elem` ["src", "href", "data", "poster"])


--------------------------------------------------------------------------------
-- | Extract URLs from tags' attributes. Those would be the same URLs on which
-- `withUrls` would act.
getUrls :: [TS.Tag String] -> [String]
getUrls tags = [u | TS.TagOpen _ as <- tags, (k, v) <- as, u <- extractUrls k v]
  where
  extractUrls "srcset" value =
    let srcset = fmap unSrcset $ P.parse srcsetParser "" value
    in map srcsetImageCandidateUrl $ fromRight [] srcset
  extractUrls key value
    | isUrlAttribute key = [value]
    | otherwise = []


--------------------------------------------------------------------------------
-- | Apply a function to each URL on a webpage
withUrls :: (String -> String) -> String -> String
withUrls f = withTags tag
  where
    tag (TS.TagOpen s a) = TS.TagOpen s $ map attr a
    tag x                = x

    attr input@("srcset", v)   =
      case fmap unSrcset $ P.parse srcsetParser "" v of
        Right srcset ->
          let srcset' = map (\i -> i { srcsetImageCandidateUrl = f $ srcsetImageCandidateUrl i }) srcset
              srcset'' = show $ Srcset srcset'
          in ("srcset", srcset'')
        Left _ -> input
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
toUrl url = case (removeWinPathSeparator url) of
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
toSiteRoot = removeWinPathSeparator . emptyException . joinPath
           . map parent . filter relevant . splitPath . takeDirectory
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


--------------------------------------------------------------------------------
data Srcset = Srcset {
    unSrcset :: [SrcsetImageCandidate]
  }


--------------------------------------------------------------------------------
instance Show Srcset where
  show set = intercalate ", " $ map show $ unSrcset set


--------------------------------------------------------------------------------
data SrcsetImageCandidate = SrcsetImageCandidate {
    srcsetImageCandidateUrl :: String
  , srcsetImageCandidateDescriptor :: Maybe String
  }


--------------------------------------------------------------------------------
instance Show SrcsetImageCandidate where
  show candidate =
    let url = srcsetImageCandidateUrl candidate
    in case srcsetImageCandidateDescriptor candidate of
      Just desc -> concat [url, " ", desc]
      Nothing -> url


--------------------------------------------------------------------------------
-- HTML spec: https://html.spec.whatwg.org/#srcset-attributes
srcsetParser :: P.Parsec String () Srcset
srcsetParser = do
  result <- candidate `P.sepBy1` (PC.char ',')
  P.eof
  return $ Srcset result
  where
  candidate :: P.Parsec String () SrcsetImageCandidate
  candidate = do
    P.skipMany ascii_whitespace
    u <- url
    P.skipMany ascii_whitespace
    desc <- P.optionMaybe $ P.choice $ fmap P.try [width_descriptor, px_density_descriptor]
    P.skipMany ascii_whitespace
    return $ SrcsetImageCandidate {
        srcsetImageCandidateUrl = u
      , srcsetImageCandidateDescriptor = desc
      }

  -- This is an over-simplification, but should be good enough for our purposes
  url :: P.Parsec String () String
  url = P.many1 $ PC.noneOf " ,"

  ascii_whitespace :: P.Parsec String () ()
  ascii_whitespace = void $ P.oneOf "\x09\x0A\x0C\x0D\x20"

  width_descriptor :: P.Parsec String () String
  width_descriptor = do
    number <- P.many1 PC.digit
    void $ PC.char 'w'
    return $ concat [number, "w"]

  px_density_descriptor :: P.Parsec String () String
  px_density_descriptor = do
    sign <- P.optionMaybe $ PC.char '-'
    int <- P.many1 PC.digit
    frac <- P.optionMaybe $ do
      void $ PC.char '.'
      frac <- P.many1 PC.digit
      return $ concat [".", frac]
    expon <- P.optionMaybe $ do
      letter <- P.oneOf "eE"
      e_sign <- P.optionMaybe $ PC.oneOf "-+"
      number <- P.many1 PC.digit
      return $ concat [[letter], mb $ fmap show e_sign, number]
    void $ PC.char 'x'
    return $ concat [mb $ fmap show sign, int, mb frac, mb expon, "x"]

  mb :: Maybe String -> String
  mb = fromMaybe ""
