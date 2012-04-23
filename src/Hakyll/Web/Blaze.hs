-- | Module providing BlazeHtml support for hakyll
--
module Hakyll.Web.Blaze
    ( getFieldHtml
    , getFieldHtml'
    , getBodyHtml
    , getBodyHtml'
    ) where

import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Internal (preEscapedString)

import Hakyll.Web.Page
import Hakyll.Web.Page.Metadata

-- | Get a field from a page and convert it to HTML. This version does not
-- escape the given HTML
--
getFieldHtml :: String -> Page a -> Html
getFieldHtml key = preEscapedString . getField key

-- | Version of 'getFieldHtml' that escapes the HTML content
--
getFieldHtml' :: String -> Page a -> Html
getFieldHtml' key = toHtml . getField key

-- | Get the body as HTML
--
getBodyHtml :: Page String -> Html
getBodyHtml = preEscapedString . pageBody

-- | Version of 'getBodyHtml' that escapes the HTML content
--
getBodyHtml' :: Page String -> Html
getBodyHtml' = toHtml . pageBody
