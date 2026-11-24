--------------------------------------------------------------------------------
-- | Provides utilities to manipulate HTML pages
module Hakyll.Web.Html
    ( -- * Generic
      withTags
    , withTagList
    , withTagListM

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
import           Control.Monad                   (unless, void, when)
import           Control.Monad.Identity          (Identity(runIdentity))
import           Data.Char                       (digitToInt, intToDigit,
                                                  isDigit, toLower)
import           Data.Either                     (fromRight)
import           Data.List                       (dropWhileEnd, intercalate,
                                                  isPrefixOf, isSuffixOf,
                                                  stripPrefix)
import           Data.Maybe                      (fromMaybe, isNothing)
import qualified Data.Set                        as S
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
withTagList f = runIdentity . withTagListM (pure . f)

-- | Map over all tags (as list) in the document, in a monad
withTagListM :: Monad m => ([TS.Tag String] -> m [TS.Tag String]) -> String -> m String
withTagListM f = fmap renderTags' . f . parseTags'
{-# INLINE withTagListM #-}

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
      case fmap unSrcset $ P.parse srcsetParser "" (decodeSrcsetReferences v) of
        Right srcset ->
          let srcset' = map (\i -> i { srcsetImageCandidateUrl = f $ srcsetImageCandidateUrl i }) srcset
              srcset'' = show $ Srcset srcset'
          in ("srcset", srcset'')
        Left _ -> input
    attr (k, v)          = (k, if isUrlAttribute k then f v else v)


--------------------------------------------------------------------------------
-- | parseTags' keeps named references; decode those for characters that
-- srcset's syntax uses.
decodeSrcsetReferences :: String -> String
decodeSrcsetReferences ('&' : s)
    | Just rest <- stripPrefix "Tab;" s     = '\t' : decodeSrcsetReferences rest
    | Just rest <- stripPrefix "NewLine;" s = '\n' : decodeSrcsetReferences rest
    | Just rest <- stripPrefix "comma;" s   = ',' : decodeSrcsetReferences rest
    | Just rest <- stripPrefix "period;" s  = '.' : decodeSrcsetReferences rest
    | Just rest <- stripPrefix "plus;" s    = '+' : decodeSrcsetReferences rest
decodeSrcsetReferences (c : s)              = c : decodeSrcsetReferences s
decodeSrcsetReferences []                   = []


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
  P.skipMany separator
  result <- P.many1 $ candidate <* P.skipMany separator
  P.eof
  return $ Srcset result
  where
  separator :: P.Parsec String () ()
  separator = ascii_whitespace P.<|> void (PC.char ',')

  candidate :: P.Parsec String () SrcsetImageCandidate
  candidate = do
    u <- url
    -- A URL may contain commas; trailing commas end the candidate
    if "," `isSuffixOf` u
      then return $ SrcsetImageCandidate {
          srcsetImageCandidateUrl = dropWhileEnd (== ',') u
        , srcsetImageCandidateDescriptor = Nothing
        }
      else do
        P.skipMany ascii_whitespace
        descs <- P.many $ descriptor <* P.skipMany ascii_whitespace
        unless (valid_descriptors $ map fst descs) $ P.parserFail "descriptors"
        P.eof P.<|> void (PC.char ',')
        return $ SrcsetImageCandidate {
            srcsetImageCandidateUrl = u
          , srcsetImageCandidateDescriptor =
              if null descs then Nothing else Just (unwords $ map snd descs)
          }

  descriptor :: P.Parsec String () (Char, String)
  descriptor = P.choice $ fmap (\(k, d) -> P.try $ (,) k <$> d <* token_end)
    [('w', width_descriptor), ('x', px_density_descriptor), ('h', height_descriptor)]

  token_end :: P.Parsec String () ()
  token_end = P.lookAhead $ ascii_whitespace P.<|> void (PC.char ',') P.<|> P.eof

  -- At most one of each; a width or a density, and a height only with a width
  valid_descriptors :: String -> Bool
  valid_descriptors ks =
    all (\k -> length (filter (== k) ks) <= 1) "wxh" &&
    not ('w' `elem` ks && 'x' `elem` ks) &&
    ('h' `notElem` ks || 'w' `elem` ks)

  url :: P.Parsec String () String
  url = P.many1 $ PC.noneOf ascii_whitespace_chars

  ascii_whitespace :: P.Parsec String () ()
  ascii_whitespace = void $ P.oneOf ascii_whitespace_chars

  ascii_whitespace_chars :: String
  ascii_whitespace_chars = "\x09\x0A\x0C\x0D\x20"

  width_descriptor :: P.Parsec String () String
  width_descriptor = do
    number <- P.many1 PC.digit
    void $ PC.char 'w'
    unless (positive_int number) $ P.parserFail "width"
    return $ concat [number, "w"]

  height_descriptor :: P.Parsec String () String
  height_descriptor = do
    number <- P.many1 PC.digit
    void $ PC.char 'h'
    unless (positive_int number) $ P.parserFail "height"
    return $ concat [number, "h"]

  px_density_descriptor :: P.Parsec String () String
  px_density_descriptor = do
    sign <- P.optionMaybe $ PC.char '-'
    int <- P.many PC.digit
    frac <- P.optionMaybe $ do
      void $ PC.char '.'
      frac <- P.many1 PC.digit
      return $ concat [".", frac]
    when (null int && isNothing frac) $ P.parserFail "density"
    expon <- P.optionMaybe $ do
      letter <- P.oneOf "eE"
      e_sign <- P.optionMaybe $ PC.oneOf "-+"
      number <- P.many1 PC.digit
      return $ concat [[letter], mb $ fmap pure e_sign, number]
    void $ PC.char 'x'
    return $ concat [mb $ fmap pure sign, int, mb frac, mb expon, "x"]

  mb :: Maybe String -> String
  mb = fromMaybe ""

  -- Positive, and within 32 bits as browsers require
  positive_int :: String -> Bool
  positive_int number = case dropWhile (== '0') number of
    []     -> False
    digits -> length digits < 10 || (length digits == 10 && digits <= "2147483647")
