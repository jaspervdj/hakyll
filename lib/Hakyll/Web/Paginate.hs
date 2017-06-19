--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Paginate
    ( PageNumber
    , Paginate (..)
    , buildPaginateWith
    , paginateEvery
    , paginateRules
    , paginateContext
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            (empty)
import           Control.Monad                  (forM_, forM)
import qualified Data.Map                       as M
import qualified Data.Set                       as S


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
import           Hakyll.Core.Rules
import           Hakyll.Web.Html
import           Hakyll.Web.Template.Context


--------------------------------------------------------------------------------
type PageNumber = Int


--------------------------------------------------------------------------------
-- | Data about paginators
data Paginate = Paginate
    { paginateMap        :: M.Map PageNumber [Identifier]
    , paginateMakeId     :: PageNumber -> Identifier
    , paginateDependency :: Dependency
    }


--------------------------------------------------------------------------------
paginateNumPages :: Paginate -> Int
paginateNumPages = M.size . paginateMap


--------------------------------------------------------------------------------
paginateEvery :: Int -> [a] -> [[a]]
paginateEvery n = go
  where
    go [] = []
    go xs = let (y, ys) = splitAt n xs in y : go ys


--------------------------------------------------------------------------------
buildPaginateWith
    :: MonadMetadata m
    => ([Identifier] -> m [[Identifier]])  -- ^ Group items into pages
    -> Pattern                             -- ^ Select items to paginate
    -> (PageNumber -> Identifier)          -- ^ Identifiers for the pages
    -> m Paginate
buildPaginateWith grouper pattern makeId = do
    ids      <- getMatches pattern
    idGroups <- grouper ids
    let idsSet = S.fromList ids
    return Paginate
        { paginateMap        = M.fromList (zip [1 ..] idGroups)
        , paginateMakeId     = makeId
        , paginateDependency = PatternDependency pattern idsSet
        }


--------------------------------------------------------------------------------
paginateRules :: Paginate -> (PageNumber -> Pattern -> Rules ()) -> Rules ()
paginateRules paginator rules =
    forM_ (M.toList $ paginateMap paginator) $ \(idx, identifiers) ->
        rulesExtraDependencies [paginateDependency paginator] $
            create [paginateMakeId paginator idx] $
                rules idx $ fromList identifiers


--------------------------------------------------------------------------------
-- | Get the identifier for a certain page by passing in the page number.
paginatePage :: Paginate -> PageNumber -> Maybe Identifier
paginatePage pag pageNumber
    | pageNumber < 1                      = Nothing
    | pageNumber > (paginateNumPages pag) = Nothing
    | otherwise                           = Just $ paginateMakeId pag pageNumber


--------------------------------------------------------------------------------
-- | A default paginate context which provides the following keys:
--
--
-- * @firstPageNum@
-- * @firstPageUrl@
-- * @previousPageNum@
-- * @previousPageUrl@
-- * @nextPageNum@
-- * @nextPageUrl@
-- * @lastPageNum@
-- * @lastPageUrl@
-- * @currentPageNum@
-- * @currentPageUrl@
-- * @numPages@
-- * @allPages@
paginateContext :: Paginate -> PageNumber -> Context a
paginateContext pag currentPage = mconcat
    [ field "firstPageNum"    $ \_ -> otherPage 1                 >>= num
    , field "firstPageUrl"    $ \_ -> otherPage 1                 >>= url
    , field "previousPageNum" $ \_ -> otherPage (currentPage - 1) >>= num
    , field "previousPageUrl" $ \_ -> otherPage (currentPage - 1) >>= url
    , field "nextPageNum"     $ \_ -> otherPage (currentPage + 1) >>= num
    , field "nextPageUrl"     $ \_ -> otherPage (currentPage + 1) >>= url
    , field "lastPageNum"     $ \_ -> otherPage lastPage          >>= num
    , field "lastPageUrl"     $ \_ -> otherPage lastPage          >>= url
    , field "currentPageNum"  $ \i -> thisPage i                  >>= num
    , field "currentPageUrl"  $ \i -> thisPage i                  >>= url
    , constField "numPages"   $ show $ paginateNumPages pag
    , Context $ \k _ i -> case k of
        "allPages" -> do
            let ctx =
                    field "isCurrent" (\n -> if fst (itemBody n) == currentPage then return "true" else empty) `mappend`
                    field "num" (num . itemBody) `mappend`
                    field "url" (url . itemBody)

            list <- forM [1 .. lastPage] $
                \n -> if n == currentPage then thisPage i else otherPage n
            items <- mapM makeItem list
            return $ ListField ctx items
        _          -> do
            empty

    ]
  where
    lastPage = paginateNumPages pag

    thisPage i = return (currentPage, itemIdentifier i)
    otherPage n
        | n == currentPage = fail $ "This is the current page: " ++ show n
        | otherwise        = case paginatePage pag n of
            Nothing -> fail $ "No such page: " ++ show n
            Just i  -> return (n, i)

    num :: (Int, Identifier) -> Compiler String
    num = return . show . fst

    url :: (Int, Identifier) -> Compiler String
    url (n, i) = getRoute i >>= \mbR -> case mbR of
        Just r  -> return $ toUrl r
        Nothing -> fail $ "No URL for page: " ++ show n
