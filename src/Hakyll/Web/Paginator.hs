{-# LANGUAGE OverloadedStrings          #-}

module Hakyll.Web.Paginator
   ( Paginator(..)
   , PagState(..)
   , NavigationLinkType(..)
   , buildPaginator
   , buildPaginatorWith
   , paginatorRules
   , renderPaginator
   , renderPaginatorWith

   , paginatorFields
  ) where

--------------------------------------------------------------------------------
import           Control.Monad                   (forM, forM_)
import           Data.List                       (intercalate, unfoldr)
import           Data.Monoid                     ((<>))
import           Data.Maybe                      (fromMaybe)
import qualified Data.Map                        as M
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Printf                     (printf)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Metadata
import           Hakyll.Core.Rules
import           Hakyll.Web.Template.Context
import           Hakyll.Core.Item
import           Hakyll.Web.Html

-- | Data about paginators
data Paginator = Paginator
    { pagPages  :: M.Map Int [Identifier]
    , pagPlaces :: M.Map Identifier Int
    , pagMakeId :: PagState -> Identifier
    , pagDependency :: Dependency
    } deriving (Show)

data PagState = PagState { pagPos :: Int
                         , pagLen :: Int }

buildPaginatorWith :: MonadMetadata m
                   => Int
                   -> (PagState -> Identifier)
                   -> Pattern
                   -> m Paginator
buildPaginatorWith n makeId pattern = do
  -- TODO: there is no sensible order for `ids` here, for now it's random;
  -- but it should be `resectFirst` order because most recent posts should
  -- correspond to 1st paginator page and oldest one to last page
  idents <- getMatches pattern
  let pages = unfoldr f idents
        where
          f [] = Nothing
          f x  = Just $ splitAt n x
      nPages     = length pages
      pagPages'  = zip [1..] pages
      pagPlaces' = [(ident, idx) | (idx,ids) <- pagPages', ident <- ids] ++
                   [(makeId (PagState i nPages), i) | i <- [1 .. nPages]]
  return $ Paginator (M.fromList pagPages') (M.fromList pagPlaces') makeId
                     (PatternDependency pattern idents)

--------------------------------------------------------------------------------

buildPaginator :: MonadMetadata m => Pattern -> m Paginator
buildPaginator = buildPaginatorWith 5 makeId
  where
    makeId (PagState pos n) = fromFilePath $ "index" ++ makeIndex pos n ++ ".html"
    makeIndex i n = let nils = replicate (length (show n) - length (show i)) '0'
                    in  nils ++ show i

--------------------------------------------------------------------------------

paginatorRules :: Paginator -> (PagState -> Pattern -> Rules ()) -> Rules ()
paginatorRules paginator rules =
    forM_ (M.toList $ pagPages paginator) $ \(idx, identifiers) ->
        let pagState = PagState idx (M.size $ pagPages paginator)
        in  create [pagMakeId paginator pagState] $
                rulesExtraDependencies [pagDependency paginator] $
                    rules pagState $ fromList identifiers

--------------------------------------------------------------------------------

data NavigationLinkType = NFirst | NPrev | NNext | NLast

instance Show NavigationLinkType where
  show NFirst = "first"
  show NPrev  = "prev"
  show NNext  = "next"
  show NLast  = "last"

renderPaginatorWith :: (String -> Int -> Int -> Int -> String)
                    -- ^ Produce a paginator menu item: url, index of menu element,
                    --   index of current page, amount of pages
                    -> (String -> NavigationLinkType -> Int -> String)
                    -- ^ Produce fast navigation links: url, type of navigation
                    --   element (e.g. last, prev...), index of corresponding page
                    -> ([String] -> String)
                    -- ^ Join items
                    -> Paginator
                    -> PagState
                    -> Compiler String
renderPaginatorWith makeHtml navigationHtml concatHtml paginator (PagState i n) = do
    pags' <- forM (M.toList $ pagPages paginator) $ \(idx,_) -> do
        let pagState = PagState idx (M.size $ pagPages paginator)
            
        url <- getRoute $ pagMakeId paginator pagState
        return (url, idx)

    let -- Create a link for one item
        makeHtml' (url, idx) =
            makeHtml (toUrl $ fromMaybe "/" url) idx i n

        -- Fast-travel links logic: first, prev, next, last (<< < > >>)
        navIdxs = [1, max 1 (i-1), min (i+1) n, n]
        navIds  = [NFirst, NPrev, NNext, NLast]
        navUrls = map (\idx -> toFilePath $ pagMakeId paginator (PagState idx n))
                     navIdxs
        navHtmlCode = zipWith3 navigationHtml navUrls navIds navIdxs
        navLefts = if i==1
                   then []
                   else take 2 navHtmlCode
        navRights= if i==n
                   then []
                   else drop 2 navHtmlCode

    return $ concatHtml $ navLefts ++ map makeHtml' pags' ++ navRights

renderPaginator :: Paginator -> PagState -> Compiler String
renderPaginator =
  renderPaginatorWith makeHtml navigationHtml concatHtml
  where
    navigationHtml url navType _idx =
      let (caption, alt) = arrow navType
      in  renderHtml $ H.a ! (A.href  (toValue url) <>
                              A.title (toValue alt))
                           $ toHtml caption
      where
        arrow :: NavigationLinkType -> (String, String)
        arrow NPrev  = ("<" , "prev")
        arrow NNext  = (">" , "next")
        arrow NFirst = ("<<", "first")
        arrow NLast  = (">>", "last")
    concatHtml = intercalate " " . filter (not . null)
    makeHtml url menuItemIdx pageIdx nPages
      | menuItemIdx == pageIdx = show menuItemIdx
      | not shouldBeDisplayed = ""
      | otherwise =
        let caption = show menuItemIdx
        in  renderHtml $ H.a ! (A.href  (toValue url) <>
                                A.title (toValue caption))
                             $ toHtml caption
      where
        shouldBeDisplayed =
          let leg = 2
              width = 1 + 2*leg
          in    abs (menuItemIdx - pageIdx) <= leg
             || pageIdx - leg <= 0      && menuItemIdx - width <= 0
             || pageIdx + leg >= nPages && menuItemIdx + width >= nPages


paginatorField :: Paginator -> String -> NavigationLinkType -> Context a
paginatorField pag fieldName arrowType = field fieldName $ \item -> do
    let identifier = itemIdentifier item
        nPages     = M.size (pagPages pag)
        
        neededPage NNext pos | pos+1 > nPages = Nothing
        neededPage NNext pos = Just (pos + 1)
        
        neededPage NPrev pos | pos-1 < 1 = Nothing
        neededPage NPrev pos = Just (pos - 1)

        neededPage NFirst pos | pos == 1 = Nothing
        neededPage NFirst  _  = Just 1

        neededPage NLast pos | pos == nPages = Nothing
        neededPage NLast  _  = Just nPages
    
    case M.lookup identifier (pagPlaces pag) of
      Nothing -> error $ printf "Hakyll.Web.Paginator: there is no page %s in paginator map."
                         (show identifier)
      Just pos -> case neededPage arrowType pos of
        Nothing   -> fail $ printf "There is no %s page for page %s in position %s."
                            (show arrowType) (show identifier) (show pos)
        Just pos' -> do
          let nextId = pagMakeId pag (PagState pos' nPages)
          mroute <- getRoute nextId
          case mroute of
            Nothing -> error $ printf "Hakyll.Web.Paginator: unable to get route of identifier %s."
                               (show nextId)
            Just rt -> return $ toUrl rt

paginatorFields :: Paginator -> Context a
paginatorFields pag =    paginatorField pag "firstPage" NFirst
                      <> paginatorField pag "prevPage"  NPrev
                      <> paginatorField pag "nextPage"  NNext
                      <> paginatorField pag "lastPage"  NLast
                      