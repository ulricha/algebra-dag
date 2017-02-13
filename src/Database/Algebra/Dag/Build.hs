{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Algebra.Dag.Build
    ( Build
    , BuildT
    , BuildState
    , runBuild
    , runBuildT
    , tag
    , insert
    , insertNoShare
    ) where

import           Control.Monad.State
import qualified Data.IntMap                 as IM

import qualified Database.Algebra.Dag        as Dag
import           Database.Algebra.Dag.Common


data BuildState alg = BuildState
    { dag  :: Dag.AlgebraDag alg  -- ^ The operator DAG that is built
    , tags :: NodeMap [Tag]       -- ^ Tags for nodes
    }

-- | The DAG builder monad, abstracted over the algebra stored in the
-- DAG. Internally, the monad detects sharing of subgraphs via hash
-- consing.
type Build alg = State (BuildState alg)

type BuildT alg m = StateT (BuildState alg) m

-- | Evaluate a monadic DAG construction.
runBuild :: Build alg r -> (Dag.AlgebraDag alg, r, NodeMap [Tag])
runBuild m = (dag s, r, tags s)
  where
    initialBuildState = BuildState { dag = Dag.emptyDag, tags = IM.empty }
    (r, s)            = runState m initialBuildState

-- | Evaluate a monadic DAG construction (transformer).
runBuildT :: Monad m => StateT (BuildState alg) m a -> m (Dag.AlgebraDag alg, a, NodeMap [Tag])
runBuildT ma = do
    let initialBuildState = BuildState { dag = Dag.emptyDag, tags = IM.empty }
    (a, s) <- runStateT ma initialBuildState
    pure (dag s, a, tags s)

-- | Tag a subtree with a comment
tag :: MonadState (BuildState alg) m => String -> AlgNode -> m AlgNode
tag msg c = do
    modify $ \s -> s { tags = IM.insertWith (++) c [msg] $ tags s }
    return c

-- | Insert a node into the graph construction environment, first check if the node already exists
-- | if so return its id, otherwise insert it and return its id.
insert :: (Dag.Operator alg, MonadState (BuildState alg) m) => alg -> m AlgNode
insert op = do
    d <- gets dag
    let (n, d') = Dag.insert op d
    modify $ \s -> s { dag = d' }
    return n

insertNoShare :: (Dag.Operator alg, MonadState (BuildState alg) m) => alg -> m AlgNode
insertNoShare op = do
    d <- gets dag
    let (n, d') = Dag.insertNoShare op d
    modify $ \s -> s { dag = d' }
    return n
