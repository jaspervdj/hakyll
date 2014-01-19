--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Hakyll.Core.Rules.Internal
    ( RulesRead (..)
    , RulesItem (..)
    , RulesWrite (..)
    , RuleSet (..)
    , RulesState (..)
    , emptyRulesState
    , Rules (..)
    , runRules
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            (Applicative, (<$>))
import           Control.Monad                  (foldM)
import           Control.Monad.Error            (ErrorT, MonadError, throwError)
import           Control.Monad.Reader           (ask)
import           Control.Monad.RWS              (RWST, runRWST)
import           Control.Monad.Trans            (lift, liftIO)
import           Data.List                      (foldl')
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Monoid                    (Monoid, mappend, mempty)
import           Data.Set                       (Set)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Item.SomeItem
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider
import           Hakyll.Core.Routes


--------------------------------------------------------------------------------
data RulesRead = RulesRead
    { rulesProvider     :: Provider
    , rulesPattern      :: Maybe Pattern
    , rulesCreate       :: [Identifier]
    , rulesVersion      :: Maybe String
    , rulesDependencies :: [Dependency]
    }


--------------------------------------------------------------------------------
data RulesItem = RulesItem (Maybe Routes) (Maybe (Compiler SomeItem))


--------------------------------------------------------------------------------
data RulesWrite = RulesWrite
    { rulesCreated :: [(Identifier, Maybe String, RulesItem)]
    , rulesMatched :: [(Pattern, Maybe String, RulesItem)]
    }


--------------------------------------------------------------------------------
instance Monoid RulesWrite where
    mempty                                        = RulesWrite mempty mempty
    mappend (RulesWrite c1 m1) (RulesWrite c2 m2) = RulesWrite
        (mappend c1 c2) (mappend m1 m2)


--------------------------------------------------------------------------------
data RuleSet = RuleSet
    { -- | Accumulated compilers
      rulesItems        :: Map Identifier (Maybe Routes, Compiler SomeItem)
    , -- | A set of the actually used files
      rulesResources    :: Set Identifier
    , -- | A pattern we can use to check if a file *would* be used. This is
      -- needed for the preview server.
      rulesTotalPattern :: Pattern
    }


--------------------------------------------------------------------------------
data RulesState = RulesState
    { rulesRoute    :: Maybe Routes
    , rulesCompiler :: Maybe (Compiler SomeItem)
    }


--------------------------------------------------------------------------------
emptyRulesState :: RulesState
emptyRulesState = RulesState Nothing Nothing


--------------------------------------------------------------------------------
-- | The monad used to compose rules
newtype Rules a = Rules
    { unRules :: RWST RulesRead RulesWrite RulesState IO a
    } deriving (Monad, Functor, Applicative)


--------------------------------------------------------------------------------
instance MonadMetadata Rules where
    getMetadata identifier = Rules $ do
        provider <- rulesProvider <$> ask
        liftIO $ resourceMetadata provider identifier

    getMatches pattern = Rules $ do
        provider <- rulesProvider <$> ask
        return $ filterMatches pattern $ resourceList provider


--------------------------------------------------------------------------------
-- | Run a Rules monad, resulting in a 'RuleSet'
runRules :: Rules a -> Provider -> IO RuleSet
runRules = undefined


--------------------------------------------------------------------------------
processRules :: Rules a -> Provider -> ErrorT String IO RuleSet
processRules rules provider = do
    -- Start by extracting the written stuff
    (_, _, rs) <- lift $ runRWST (unRules rules) env emptyRulesState

    -- The created compilers are easy. We just insert those.
    compilers0 <- foldM
        (\m (k, ri) -> insertRulesItem k ri m)
        M.empty [(overrideVersion v k, ri) | (k, v, ri) <- rulesCreated rs]

    -- j
    

    undefined

    {-
     - rules provider = do
    (_, _, ruleSet) <- runRWST (unRules rules) env emptyRulesState

    -- Ensure compiler uniqueness
    let ruleSet' = ruleSet
            { rulesCompilers = M.toList $
                M.fromListWith (flip const) (rulesCompilers ruleSet)
            }

    return ruleSet'
    -}
  where
    overrideVersion Nothing  i = i
    overrideVersion (Just v) i = setVersion (Just v) i

    env = RulesRead
        { rulesProvider     = provider
        , rulesPattern      = Nothing
        , rulesCreate       = []
        , rulesVersion      = Nothing
        , rulesDependencies = []
        }


--------------------------------------------------------------------------------
unionRulesItem :: RulesItem -> RulesItem -> Either String RulesItem
unionRulesItem (RulesItem r1 c1) (RulesItem r2 c2) = do
    r <- union "duplicate Routes"   r1 r2
    c <- union "duplicate Compiler" c1 c2
    return $ RulesItem r c
  where
    union _ Nothing  Nothing  = Right Nothing
    union _ (Just x) Nothing  = Right (Just x)
    union _ Nothing  (Just x) = Right (Just x)
    union e (Just _) (Just _) = Left e


--------------------------------------------------------------------------------
insertRulesItem :: MonadError String m
                => Identifier -> RulesItem -> Map Identifier RulesItem
                -> m (Map Identifier RulesItem)
insertRulesItem k ri1 m = case M.lookup k m of
    Nothing  -> return $ M.insert k ri1 m
    Just ri2 -> case unionRulesItem ri1 ri2 of
        Right ri -> return $ M.insert k ri m
        Left err -> throwError $
            "Hakyll.Core.Rules.Internal.insertRulesItem: " ++
            show k ++ ": " ++ err
