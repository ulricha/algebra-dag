{-# LANGUAGE BangPatterns #-}

module Database.Algebra.Rewrite.Properties
    ( inferBottomUpG
    ) where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
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
    foldl' go M.empty $ reverse topOrderedNodes
  where
    go pm node = M.insert node (inferWorker (nodeMap dag) (operator node dag) node pm) pm
