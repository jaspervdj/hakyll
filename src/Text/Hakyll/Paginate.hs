-- | Module aimed to paginate web pages.
module Text.Hakyll.Paginate
    ( PaginateConfiguration (..)
    , defaultPaginateConfiguration
    , paginate
    ) where

import Control.Applicative ((<$>))

import Text.Hakyll.Renderables
import Text.Hakyll.RenderAction
import Text.Hakyll.Util (link)

-- | A configuration for a pagination.
data PaginateConfiguration = PaginateConfiguration
    { -- | Label for the link to the previous page.
      previousLabel :: String
    , -- | Label for the link to the next page.
      nextLabel     :: String
    , -- | Label for the link to the first page.
      firstLabel    :: String
    , -- | Label for the link to the last page.
      lastLabel     :: String
    }

-- | A simple default configuration for pagination.
defaultPaginateConfiguration :: PaginateConfiguration
defaultPaginateConfiguration = PaginateConfiguration
    { previousLabel = "Previous"
    , nextLabel     = "Next"
    , firstLabel    = "First"
    , lastLabel     = "Last"
    }

-- | The most important function for pagination. This function operates on a
--   list of renderables (the pages), and basically just adds fields to them
--   by combining them with a custom page.
--
--   The following metadata fields will be added:
--
--   - @$previous@: A link to the previous page.
--
--   - @$next@: A link to the next page.
--
--   - @$first@: A link to the first page.
--
--   - @$last@: A link to the last page.
--
--   - @$index@: 1-based index of the current page.
--
--   - @$length@: Total number of pages.
--
--   When @$previous@ or @$next@ are not available, they will be just a label
--   without a link. The same goes for when we are on the first or last page for
--   @$first@ and @$last@.
paginate :: PaginateConfiguration
         -> [Renderable]
         -> [Renderable]
paginate configuration renderables = paginate' Nothing renderables (1 :: Int)
  where
    -- Create a link with a given label, taken from the configuration.
    linkWithLabel f r = Right $ case actionUrl r of
        Just l  -> createSimpleRenderAction $ link (f configuration) <$> l
        Nothing -> error "No link found for pagination."

    -- The main function that creates combined renderables by recursing over
    -- the list of renderables.
    paginate' _ [] _ = []
    paginate' maybePrev (x:xs) index = 
        let (previous, first) = case maybePrev of
                (Just r) -> ( linkWithLabel previousLabel r
                            , linkWithLabel firstLabel (head renderables) )
                Nothing  -> ( Left $ previousLabel configuration
                            , Left $ firstLabel configuration )
            (next, last') = case xs of
                (n:_) -> ( linkWithLabel nextLabel n
                         , linkWithLabel lastLabel (last renderables) )
                []    -> ( Left $ nextLabel configuration
                         , Left $ lastLabel configuration )
            customPage = createCustomPage ""
                [ ("previous", previous)
                , ("next", next)
                , ("first", first)
                , ("last", last')
                , ("index", Left $ show index)
                , ("length", Left $ show $ length renderables)
                ]
        in (x `combine` customPage) : paginate' (Just x) xs (index + 1)
