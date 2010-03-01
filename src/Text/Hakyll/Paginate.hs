-- | Module aimed to paginate web pages.
module Text.Hakyll.Paginate
    ( PaginateConfiguration (..)
    , defaultPaginateConfiguration
    , paginate
    ) where

import Text.Hakyll.Renderables
import Text.Hakyll.Renderable (Renderable, getUrl)
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
--   - @$next@: A link to the next page.
--   - @$first@: A link to the first page.
--   - @$last@: A link to the last page.
--   - @$index@: 1-based index of the current page.
--   - @$length@: Total number of pages.
--
paginate :: (Renderable a)
         => PaginateConfiguration
         -> [a]
         -> [CombinedRenderable a CustomPage]
paginate configuration renderables = paginate' Nothing renderables (1 :: Int)
  where
    linkWithLabel f r = link (f configuration) `fmap` getUrl r

    first = linkWithLabel firstLabel (head renderables)
    last' = linkWithLabel lastLabel (last renderables)
    length' = length renderables

    paginate' _ [] _ = []
    paginate' maybePrev (x:xs) index = 
        let previous = case maybePrev of
                (Just r) -> linkWithLabel previousLabel r
                Nothing  -> return $ previousLabel configuration
            next = case xs of
                (n:_) -> linkWithLabel nextLabel n
                []    -> return $ nextLabel configuration
            customPage = createCustomPage "" []
                [ ("previous", Right previous)
                , ("next", Right next)
                , ("first", Right first)
                , ("last", Right last')
                , ("index", Left $ show index)
                , ("length", Left $ show length')
                ]
        in (x `combine` customPage) : paginate' (Just x) xs (index + 1)
