--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Paginate
    ( PageNumber
    , Paginate (..)
    , buildPaginate
    , buildPaginateWith
    , paginateRules
    , paginateContext
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                  (forM_)
import           Data.List                      (unfoldr)
import qualified Data.Map                       as M
import           Data.Monoid                    (mconcat)
import           Text.Printf                    (printf)


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
    { paginatePages      :: M.Map PageNumber [Identifier]
    , paginatePlaces     :: M.Map Identifier PageNumber
    , paginateMakeId     :: PageNumber -> Identifier
    , paginateDependency :: Dependency
    } deriving (Show)


--------------------------------------------------------------------------------
buildPaginate :: MonadMetadata m
              => Pattern
              -> m Paginate
buildPaginate pattern = do
    idents <- getMatches pattern
    let pagPages  = M.fromList $ zip [1 ..] (map return idents)
        pagPlaces = M.fromList $ zip idents [1 ..]
        makeId pn = case M.lookup pn pagPages of
            Just [id'] -> id'
            _          -> error $
                "Hakyll.Web.Paginate.buildPaginate: " ++
                "invalid page number: " ++ show pn

    return $ Paginate pagPages pagPlaces makeId
        (PatternDependency pattern idents)


--------------------------------------------------------------------------------
buildPaginateWith :: MonadMetadata m
                  => Int
                  -> (PageNumber -> Identifier)
                  -> Pattern
                  -> m Paginate
buildPaginateWith n makeId pattern = do
    -- TODO: there is no sensible order for `ids` here, for now it's random;
    -- but it should be `resectFirst` order because most recent posts should
    -- correspond to 1st paginator page and oldest one to last page
    idents <- getMatches pattern
    let pages          = flip unfoldr idents $ \xs ->
            if null xs then Nothing else Just (splitAt n xs)
        nPages         = length pages
        paginatePages' = zip [1..] pages
        pagPlaces'     =
            [(ident, idx) | (idx,ids) <- paginatePages', ident <- ids] ++
            [(makeId i, i) | i <- [1 .. nPages]]

    return $ Paginate (M.fromList paginatePages') (M.fromList pagPlaces') makeId
        (PatternDependency pattern idents)


--------------------------------------------------------------------------------
paginateRules :: Paginate -> (PageNumber -> Pattern -> Rules ()) -> Rules ()
paginateRules paginator rules =
    forM_ (M.toList $ paginatePages paginator) $ \(idx, identifiers) ->
        create [paginateMakeId paginator idx] $
            rulesExtraDependencies [paginateDependency paginator] $
                rules idx $ fromList identifiers


--------------------------------------------------------------------------------
-- | Takes first, current, last page and produces index of next page
type RelPage = PageNumber -> PageNumber -> PageNumber -> Maybe PageNumber


--------------------------------------------------------------------------------
paginateField :: Paginate -> String -> RelPage -> Context a
paginateField pag fieldName relPage = field fieldName $ \item ->
    let identifier = itemIdentifier item
    in case M.lookup identifier (paginatePlaces pag) of
        Nothing -> fail $ printf
            "Hakyll.Web.Paginate: there is no page %s in paginator map."
            (show identifier)
        Just pos -> case relPage 1 pos nPages of
            Nothing   -> fail "Hakyll.Web.Paginate: No page here."
            Just pos' -> do
                let nextId = paginateMakeId pag pos'
                mroute <- getRoute nextId
                case mroute of
                    Nothing -> fail $ printf
                        "Hakyll.Web.Paginate: unable to get route for %s."
                        (show nextId)
                    Just rt -> return $ toUrl rt
  where
    nPages = M.size (paginatePages pag)


--------------------------------------------------------------------------------
paginateContext :: Paginate -> Context a
paginateContext pag = mconcat
    [ paginateField pag "firstPage"
        (\f c _ -> if c <= f then Nothing else Just f)
    , paginateField pag "previousPage"
        (\f c _ -> if c <= f then Nothing else Just (c - 1))
    , paginateField pag "nextPage"
        (\_ c l -> if c >= l then Nothing else Just (c + 1))
    , paginateField pag "lastPage"
        (\_ c l -> if c >= l then Nothing else Just l)
    ]
