--------------------------------------------------------------------------------
-- | Internal rules module for types which are not exposed to the user
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
module Hakyll.Core.Rules.Internal
    ( RuleSet (..)
    , RuleState (..)
    , RuleEnvironment (..)
    , Rules (..)
    , runRules
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            (Applicative)
import           Control.Monad.RWS              (RWST, runRWST)
import qualified Data.Map                       as M
import           Data.Monoid                    (Monoid, mappend, mempty)
import           Data.Set                       (Set)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Item.SomeItem
import           Hakyll.Core.Provider
import           Hakyll.Core.Routes


--------------------------------------------------------------------------------
-- | A collection of rules for the compilation process
data RuleSet = RuleSet
    { -- | Routes used in the compilation structure
      rulesRoutes    :: Routes
    , -- | Compilation rules
      rulesCompilers :: [(Identifier, Compiler SomeItem)]
    , -- | A set of the actually used files
      rulesResources :: Set Identifier
    }


--------------------------------------------------------------------------------
instance Monoid RuleSet where
    mempty = RuleSet mempty mempty mempty
    mappend (RuleSet r1 c1 s1) (RuleSet r2 c2 s2) =
        RuleSet (mappend r1 r2) (mappend c1 c2) (mappend s1 s2)


--------------------------------------------------------------------------------
-- | Rule state
data RuleState = RuleState
    { rulesNextIdentifier :: Int
    } deriving (Show)


--------------------------------------------------------------------------------
-- | Rule environment
data RuleEnvironment = RuleEnvironment
    { rulesProvider :: Provider
    , rulesPattern  :: Pattern
    , rulesVersion  :: Maybe String
    }


--------------------------------------------------------------------------------
-- | The monad used to compose rules
newtype Rules a = Rules
    { unRules :: RWST RuleEnvironment RuleSet RuleState IO a
    } deriving (Monad, Functor, Applicative)


--------------------------------------------------------------------------------
-- | Run a Rules monad, resulting in a 'RuleSet'
runRules :: Rules a -> Provider -> IO RuleSet
runRules rules provider = do
    (_, _, ruleSet) <- runRWST (unRules rules) env state
    return $ nubCompilers ruleSet
  where
    state = RuleState {rulesNextIdentifier = 0}
    env   = RuleEnvironment
        { rulesProvider = provider
        , rulesPattern  = mempty
        , rulesVersion  = Nothing
        }


--------------------------------------------------------------------------------
-- | Remove duplicate compilers from the 'RuleSet'. When two compilers match an
-- item, we prefer the first one
nubCompilers :: RuleSet -> RuleSet
nubCompilers set = set { rulesCompilers = nubCompilers' (rulesCompilers set) }
  where
    nubCompilers' = M.toList . M.fromListWith (flip const)
