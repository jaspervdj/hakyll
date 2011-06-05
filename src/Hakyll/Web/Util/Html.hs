-- | Miscellaneous HTML manipulation functions
--
module Hakyll.Web.Util.Html
    ( stripTags
    ) where

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
