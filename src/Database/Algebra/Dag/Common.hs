{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.Algebra.Dag.Common where

import qualified Data.IntMap  as IM
import qualified Data.Map     as M
import           GHC.Generics (Generic)

import           Data.Aeson   (FromJSON, ToJSON)

-- | Identifiers for DAG nodes.
type AlgNode = Int

type AlgMap alg = M.Map alg AlgNode
type NodeMap a = IM.IntMap a

type Tag = String

-- | Tertiary, Binary, unary and leaf nodes of a relational algebra DAG.
data Algebra t b u n c = TerOp t c c c
                       | BinOp b c c
                       | UnOp u c
                       | NullaryOp n
                         deriving (Ord, Eq, Show, Read, Generic)

class (Ord a, Show a) => Operator a where
    opChildren     :: a -> [AlgNode]
    replaceOpChild :: a -> AlgNode -> AlgNode -> a

instance (ToJSON t, ToJSON b, ToJSON u, ToJSON n, ToJSON c) => ToJSON (Algebra t b u n c) where
instance (FromJSON t, FromJSON b, FromJSON u, FromJSON n, FromJSON c) => FromJSON (Algebra t b u n c) where

replaceOp :: Eq a => a -> a -> a -> a
replaceOp orig new x = if x == orig then new else x

instance (Ord t, Ord b, Ord u, Ord n, Show t, Show b, Show u, Show n) => Operator (Algebra t b u n AlgNode) where
    opChildren (TerOp _ c1 c2 c3) = [c1, c2, c3]
    opChildren (BinOp _ c1 c2) = [c1, c2]
    opChildren (UnOp _ c) = [c]
    opChildren (NullaryOp _) = []

    replaceOpChild oper old new = replaceChild old new oper
      where
        replaceChild :: forall t b u n c. Eq c => c -> c -> Algebra t b u n c -> Algebra t b u n c
        replaceChild o n (TerOp op c1 c2 c3) = TerOp op (replaceOp o n c1) (replaceOp o n c2) (replaceOp o n c3)
        replaceChild o n (BinOp op c1 c2) = BinOp op (replaceOp o n c1) (replaceOp o n c2)
        replaceChild o n (UnOp op c) = UnOp op (replaceOp o n c)
        replaceChild _ _ (NullaryOp op) = NullaryOp op

