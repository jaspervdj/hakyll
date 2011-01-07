-- | This is the module which binds it all together
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Run where

import Prelude hiding (reverse)
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Arrow ((&&&))
import Control.Monad (foldM, forM_, forM, filterM)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (mempty)
import Data.Typeable (Typeable)
import Data.Binary (Binary)
import System.FilePath ((</>))
import Control.Applicative ((<$>))
import Data.Set (Set)
import qualified Data.Set as S

import Hakyll.Core.Route
import Hakyll.Core.Identifier
import Hakyll.Core.Util.File
import Hakyll.Core.Compiler
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.ResourceProvider
import Hakyll.Core.ResourceProvider.FileResourceProvider
import Hakyll.Core.Rules
import Hakyll.Core.DirectedGraph
import Hakyll.Core.DirectedGraph.Dot
import Hakyll.Core.DirectedGraph.DependencySolver
import Hakyll.Core.Writable
import Hakyll.Core.Store
import Hakyll.Core.CompiledItem

hakyll :: Rules -> IO ()
hakyll rules = do
    store <- makeStore "_store"
    provider <- fileResourceProvider
    let ruleSet = runRules rules provider
        compilers = rulesCompilers ruleSet
    runReaderT (unHakyll (addNewCompilers [] compilers)) $
        env ruleSet provider store
  where
    env ruleSet provider store = HakyllEnvironment
        { hakyllRoute            = rulesRoute ruleSet
        , hakyllResourceProvider = provider
        , hakyllStore            = store
        , hakyllModified         = S.empty
        , hakyllObsolete         = S.empty
        }

data HakyllEnvironment = HakyllEnvironment
    { hakyllRoute            :: Route
    , hakyllResourceProvider :: ResourceProvider
    , hakyllStore            :: Store
    , hakyllModified         :: Set Identifier
    , hakyllObsolete         :: Set Identifier
    }

newtype Hakyll a = Hakyll
    { unHakyll :: ReaderT HakyllEnvironment IO a
    } deriving (Functor, Applicative, Monad)

-- | Return a set of modified identifiers
--
modified :: ResourceProvider     -- ^ Resource provider
         -> Store                -- ^ Store
         -> [Identifier]         -- ^ Identifiers to check
         -> IO (Set Identifier)  -- ^ Modified resources
modified provider store ids = fmap S.fromList $ flip filterM ids $ \id' ->
    if resourceExists provider id' then resourceModified provider id' store
                                   else return False

-- | Add a number of compilers and continue using these compilers
--
addNewCompilers :: [(Identifier, Compiler () CompileRule)]
                -- ^ Remaining compilers yet to be run
                -> [(Identifier, Compiler () CompileRule)]
                -- ^ Compilers to add
                -> Hakyll ()
addNewCompilers oldCompilers newCompilers = Hakyll $ do
    -- Get some information
    provider <- hakyllResourceProvider <$> ask
    store <- hakyllStore <$> ask

    let -- All compilers
        compilers = oldCompilers ++ newCompilers

        -- Get all dependencies for the compilers
        dependencies = flip map compilers $ \(id', compiler) ->
            let deps = runCompilerDependencies compiler provider
            in (id', deps)

        -- Create a compiler map (Id -> Compiler)
        compilerMap = M.fromList compilers

        -- Create the dependency graph
        graph = fromList dependencies

    liftIO $ writeDot "dependencies.dot" show graph

    -- Check which items are up-to-date. This only needs to happen for the new
    -- compilers
    oldModified <- hakyllModified <$> ask 
    newModified <- liftIO $ modified provider store $ map fst newCompilers
    oldObsolete <- hakyllObsolete <$> ask

    let modified' = oldModified `S.union` newModified
        
        -- Find obsolete items
        obsolete = reachableNodes (oldObsolete `S.union` modified') $
            reverse graph

         -- Solve the graph, retain only the obsolete items
        ordered = filter (`S.member` obsolete) $ solveDependencies graph

        -- Join the order with the compilers again
        orderedCompilers = map (id &&& (compilerMap M.!)) ordered

    liftIO $ putStrLn "Adding compilers..."

    -- Now run the ordered list of compilers
    local (updateEnv modified' obsolete) $
        unHakyll $ runCompilers orderedCompilers
  where
    -- Add the modified information for the new compilers
    updateEnv modified' obsolete env = env
        { hakyllModified = modified'
        , hakyllObsolete = obsolete
        }

runCompilers :: [(Identifier, Compiler () CompileRule)]
             -- ^ Ordered list of compilers
             -> Hakyll ()
             -- ^ No result
runCompilers [] = return ()
runCompilers ((id', compiler) : compilers) = Hakyll $ do
    -- Obtain information
    route' <- hakyllRoute <$> ask
    provider <- hakyllResourceProvider <$> ask
    store <- hakyllStore <$> ask
    modified' <- hakyllModified <$> ask

    let -- Determine the URL
        url = runRoute route' id'
    
        -- Check if the resource was modified
        isModified = id' `S.member` modified'

    -- Run the compiler
    result <- liftIO $ runCompiler compiler id' provider url store isModified
    liftIO $ putStrLn $ "Generated target: " ++ show id'

    case result of
        -- Compile rule for one item, easy stuff
        CompileRule compiled -> do
            case url of
                Nothing -> return ()
                Just r  -> liftIO $ do
                    putStrLn $ "Routing " ++ show id' ++ " to " ++ r
                    let path = "_site" </> r
                    makeDirectories path
                    write path compiled

            liftIO $ putStrLn ""

            -- Continue for the remaining compilers
            unHakyll $ runCompilers compilers 

        -- Metacompiler, slightly more complicated
        MetaCompileRule newCompilers -> do
            -- Actually I was just kidding, it's not hard at all
            unHakyll $ addNewCompilers compilers newCompilers
