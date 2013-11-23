{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Algebra.VL.Data where

import           GHC.Generics                (Generic)

import           Database.Algebra.Aux
import           Database.Algebra.Dag        (Operator, opChildren, replaceOpChild)
import           Database.Algebra.Dag.Common

type VL = Algebra TerOp BinOp UnOp NullOp AlgNode

data VLType = Nat | Int | Bool |  Double
            | String | Unit
            | Pair VLType VLType | VLList VLType
            deriving (Eq, Ord, Generic, Show)

type DataColumn = String
type TypedColumn = (DataColumn, VLType)
type Key = [DataColumn]
type DBCol = Int

data AggrFun = Sum DBCol
             | Min DBCol
             | Max DBCol
             | Avg DBCol
             | Count
               deriving (Eq, Ord, Show, Read, Generic)

data VecCompOp = Eq
               | Gt
               | GtE
               | Lt
               | LtE
               deriving (Eq, Ord, Generic)

data VecNumOp = Add
              | Sub
              | Div
              | Mul
              | Mod
              deriving (Eq, Ord, Generic)

data VecBoolOp = Conj
               | Disj
               deriving (Eq, Ord, Generic)

data VecOp = COp VecCompOp
           | NOp VecNumOp
           | BOp VecBoolOp
           | Like
           deriving (Eq, Ord, Generic)

data Expr1 = App1 VecOp Expr1 Expr1
           | Column1 DBCol
           | Constant1 VLVal
           deriving (Eq, Ord, Show, Generic)

newtype LeftCol = L DBCol deriving (Eq, Ord, Show, Generic)
newtype RightCol = R DBCol deriving (Eq, Ord, Show, Generic)

data Expr2 = App2 VecOp Expr2 Expr2
           | Column2Left LeftCol
           | Column2Right RightCol
           | Constant2 VLVal
           deriving (Eq, Ord, Show, Generic)

instance Show VecOp where
    show (NOp Add)  = "+"
    show (NOp Sub)  = "-"
    show (NOp Div)  = "/"
    show (NOp Mul)  = "*"
    show (NOp Mod)  = "%"
    show (COp o)    = show o
    show (BOp Conj) = "&&"
    show (BOp Disj) = "||"
    show Like       = "LIKE"

instance Show VecCompOp where
    show Eq  = "=="
    show Gt  = ">"
    show GtE = ">="
    show Lt  = "<"
    show LtE = "<="

data ISTransProj = STDescrCol
                 | STPosCol
                 | STNumber
                 deriving (Eq, Ord, Generic, Show)

data DescrProj = DescrConst Nat
               | DescrIdentity
               | DescrPosCol
               deriving (Eq, Ord, Generic, Show)

data PosProj = PosNumber
             | PosConst Nat
             | PosIdentity
             deriving (Eq, Ord, Generic, Show)

data Proj = ProjConst VLVal
          | ProjCol DBCol
          | ProjExpr Expr1
          deriving (Eq, Ord, Generic, Show)

newtype Nat = N Int deriving (Eq, Ord, Generic, Show)

instance Integral Nat where
  quot (N i1) (N i2)    = N $ quot i1 i2
  rem (N i1) (N i2)     = N $ rem i1 i2
  div (N i1) (N i2)     = N $ div i1 i2
  mod (N i1) (N i2)     = N $ mod i1 i2
  quotRem (N i1) (N i2) = let (q, r) = quotRem i1 i2 in (N q, N r)
  divMod (N i1) (N i2)  = let (d, m) = divMod i1 i2 in (N d, N m)
  toInteger (N i)       = toInteger i

instance Real Nat where
  toRational (N i) = toRational i

instance Enum Nat where
  toEnum         = N
  fromEnum (N i) = i

instance Num Nat where
  (N i1) + (N i2) = N $ i1 + i2
  (N i1) * (N i2) = N $ i1 * i2
  (N i1) - (N i2) = N $ i1 - i2
  negate (N i)    = N $ negate i
  abs (N i)       = N $ abs i
  signum (N i)    = N $ signum i
  fromInteger i   = N $ fromInteger i

data VLVal = VLInt Int
           | VLNat Nat
           | VLBool Bool
           | VLString String
           | VLDouble Double
           | VLUnit
           deriving (Eq, Ord, Generic, Show)

data NullOp = SingletonDescr
            | ConstructLiteralValue [VLType] [VLVal]
            | ConstructLiteralTable [VLType] [[VLVal]]
            | TableRef String [TypedColumn] [Key]
            deriving (Eq, Ord, Generic, Show)

data UnOp = Unique
          | UniqueL
          | Number
          | NumberL
          | NotPrim
          | NotVec
          | LengthA
          | DescToRename
          | Segment
          | Unsegment
          | VecSum VLType
          | VecAvg
          | VecMin
          | VecMinL
          | VecMax
          | VecMaxL
          | IntegerToDoubleA
          | IntegerToDoubleL
          | ReverseA -- (DBV, PropVector)
          | ReverseL -- (DBV, PropVector)
          | FalsePositions
          | R1
          | R2
          | R3
          | ProjectRename (ISTransProj, ISTransProj) -- (source, target)?

          | VLProject [Proj]
          | VLProjectA [Proj]

          | ProjectAdmin (DescrProj, PosProj)
          | SelectExpr Expr1
          | Only
          | Singleton
          | CompExpr1L Expr1
          | SelectPos1 VecCompOp Nat
          | SelectPos1L VecCompOp Nat
          | VecAggr [DBCol] [AggrFun]
    deriving (Eq, Ord, Generic, Show)


data BinOp = GroupBy    -- (DescrVector, DBV, PropVector)
           | SortWith   -- (DBV, PropVector)
           | LengthSeg
           | DistPrim   -- (DBV, PropVector)
           | DistDesc   -- (DBV, PropVector)
           | DistLift   -- (DBV, PropVector)
           | PropRename
           | PropFilter -- (DBV, PropVector)
           | PropReorder -- (DBV, PropVector)
           | Append     -- (DBV, RenameVector, RenameVector)
           | RestrictVec -- VL (DBV, RenameVector)
           | CompExpr2 Expr2
           | CompExpr2L Expr2
           | VecSumL
           | VecAvgL
           | SelectPos VecCompOp -- (DBV, RenameVector)
           | SelectPosL VecCompOp -- (DBV, RenameVector)
           | PairA
           | PairL
           | ZipL            -- (DBV, RenameVector, RenameVector)
           | CartProduct
           | CartProductL
           | EquiJoin Expr1 Expr1
           | EquiJoinL Expr1 Expr1
           | SemiJoin Expr1 Expr1
           | SemiJoinL Expr1 Expr1
           | AntiJoin Expr1 Expr1
           | AntiJoinL Expr1 Expr1
    deriving (Eq, Ord, Generic, Show)

data TerOp = CombineVec  -- (DBV, RenameVector, RenameVector)
    deriving (Eq, Ord, Generic, Show)

instance Operator VL where
    opChildren (TerOp _ c1 c2 c3) = [c1, c2, c3]
    opChildren (BinOp _ c1 c2) = [c1, c2]
    opChildren (UnOp _ c) = [c]
    opChildren (NullaryOp _) = []

    replaceOpChild oper old new = replaceChild old new oper
     where
         replaceChild :: forall t b u n c. Eq c => c -> c -> Algebra t b u n c -> Algebra t b u n c
         replaceChild o n (TerOp op c1 c2 c3) = TerOp op (replace o n c1) (replace o n c2) (replace o n c3)
         replaceChild o n (BinOp op c1 c2) = BinOp op (replace o n c1) (replace o n c2)
         replaceChild o n (UnOp op c) = UnOp op (replace o n c)
         replaceChild _ _ (NullaryOp op) = NullaryOp op
