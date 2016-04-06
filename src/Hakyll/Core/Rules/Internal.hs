--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
module Hakyll.Core.Rules.Internal
    ( RulesRead (..)
    , RuleSet (..)
    , RulesState (..)
    , emptyRulesState
    , Rules (..)
    , runRules
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Reader           (ask)
import           Control.Monad.RWS              (RWST, runRWST)
import           Control.Monad.Trans            (liftIO)
import qualified Data.Map                       as M
import           Data.Set                       (Set)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Item.SomeItem
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider
import           Hakyll.Core.Routes


--------------------------------------------------------------------------------
data RulesRead = RulesRead
    { rulesProvider :: Provider
    , rulesMatches  :: [Identifier]
    , rulesVersion  :: Maybe String
    }


--------------------------------------------------------------------------------
data RuleSet = RuleSet
    { -- | Accumulated routes
      rulesRoutes    :: Routes
    , -- | Accumulated compilers
      rulesCompilers :: [(Identifier, Compiler SomeItem)]
    , -- | A set of the actually used files
      rulesResources :: Set Identifier
    , -- | A pattern we can use to check if a file *would* be used. This is
      -- needed for the preview server.
      rulesPattern   :: Pattern
    }


--------------------------------------------------------------------------------
instance Monoid RuleSet where
    mempty = RuleSet mempty mempty mempty mempty
    mappend (RuleSet r1 c1 s1 p1) (RuleSet r2 c2 s2 p2) =
        RuleSet (mappend r1 r2) (mappend c1 c2) (mappend s1 s2) (p1 .||. p2)


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
    { unRules :: RWST RulesRead RuleSet RulesState IO a
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
runRules rules provider = do
    (_, _, ruleSet) <- runRWST (unRules rules) env emptyRulesState

    -- Ensure compiler uniqueness
    let ruleSet' = ruleSet
            { rulesCompilers = M.toList $
                M.fromListWith (flip const) (rulesCompilers ruleSet)
            }

    return ruleSet'
  where
    env = RulesRead
        { rulesProvider = provider
        , rulesMatches  = []
        , rulesVersion  = Nothing
        }
