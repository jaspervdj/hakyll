--------------------------------------------------------------------------------
-- | Internal rules module for types which are not exposed to the user
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
module Hakyll.Core.Rules.Internal
    ( RuleSet (..)
    , RuleState (..)
    , RuleEnvironment (..)
    , RulesM (..)
    , Rules
    , runRules
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            (Applicative)
import           Control.Monad.RWS              (RWST, runRWST)
import qualified Data.Map                       as M
import           Data.Monoid                    (Monoid, mappend, mempty)
import           Data.Set                       (Set)


--------------------------------------------------------------------------------
import           Hakyll.Core.CompiledItem
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.ResourceProvider
import           Hakyll.Core.Routes


--------------------------------------------------------------------------------
-- | A collection of rules for the compilation process
data RuleSet = RuleSet
    { -- | Routes used in the compilation structure
      rulesRoutes    :: Routes
    , -- | Compilation rules
      rulesCompilers :: [(Identifier (), Compiler CompiledItem)]
    , -- | A set of the actually used files
      rulesResources :: Set (Identifier ())
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
    { rulesResourceProvider :: ResourceProvider
    , rulesPattern          :: forall a. Pattern a
    , rulesVersion          :: Maybe String
    }


--------------------------------------------------------------------------------
-- | The monad used to compose rules
newtype RulesM a = RulesM
    { unRulesM :: RWST RuleEnvironment RuleSet RuleState IO a
    } deriving (Monad, Functor, Applicative)


--------------------------------------------------------------------------------
-- | Simplification of the RulesM type; usually, it will not return any
-- result.
type Rules = RulesM ()


--------------------------------------------------------------------------------
-- | Run a Rules monad, resulting in a 'RuleSet'
runRules :: RulesM a -> ResourceProvider -> IO RuleSet
runRules rules provider = do
    (_, _, ruleSet) <- runRWST (unRulesM rules) env state
    return $ nubCompilers ruleSet
  where
    state = RuleState {rulesNextIdentifier = 0}
    env   = RuleEnvironment
        { rulesResourceProvider = provider
        , rulesPattern          = mempty
        , rulesVersion          = Nothing
        }


--------------------------------------------------------------------------------
-- | Remove duplicate compilers from the 'RuleSet'. When two compilers match an
-- item, we prefer the first one
nubCompilers :: RuleSet -> RuleSet
nubCompilers set = set { rulesCompilers = nubCompilers' (rulesCompilers set) }
  where
    nubCompilers' = M.toList . M.fromListWith (flip const)
