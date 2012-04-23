-- | Miscellaneous HTML manipulation functions
--
module Hakyll.Web.Util.Html
    ( stripTags
    , escapeHtml
    ) where

import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)

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
--
stripTags :: String -> String
stripTags []         = []
stripTags ('<' : xs) = stripTags $ drop 1 $ dropWhile (/= '>') xs
stripTags (x : xs)   = x : stripTags xs

-- | HTML-escape a string
--
-- Example:
--
-- > escapeHtml "Me & Dean"
--
-- Result:
--
-- > "Me &amp; Dean"
--
escapeHtml :: String -> String
escapeHtml = renderHtml . toHtml
