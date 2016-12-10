{-# LANGUAGE BangPatterns #-}

module Database.Algebra.Rewrite.Properties
    ( inferBottomUpG
    , inferBottomUpE
    ) where

import           Control.Monad.Except
import qualified Data.Foldable               as F
import qualified Data.IntMap                 as M
import           Data.List
import           Database.Algebra.Dag
import           Database.Algebra.Dag.Common

inferBottomUpG :: Operator o
               => (NodeMap o -> o -> AlgNode -> NodeMap p -> p)
               -> [AlgNode]
               -> AlgebraDag o
               -> NodeMap p
inferBottomUpG inferWorker topOrderedNodes dag =
    foldl' go M.empty $ topOrderedNodes
  where
    go pm node = M.insert node (inferWorker (nodeMap dag) (operator node dag) node pm) pm

-- | Infer properties for all nodes in a bottom-up traversal. Property inference
-- might fail.
inferBottomUpE :: (Operator o, MonadError e m)
               => (NodeMap o -> o -> AlgNode -> NodeMap p -> m p)
               -> [AlgNode]
               -> AlgebraDag o
               -> m (NodeMap p)
inferBottomUpE inferWorker topOrderedNodes dag =
    F.foldrM go M.empty topOrderedNodes
  where
    go n pm = do
        p <- inferWorker (nodeMap dag) (operator n dag) n pm
        pure $ M.insert n p pm
