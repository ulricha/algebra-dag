{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module Ferry.TypedCore.Data.Type where

import Ferry.TypedCore.Data.Base

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

type TyEnv = M.Map Ident TyScheme

data TyScheme where
    Forall :: Int -> Qual FType -> TyScheme
 deriving Show

infix 5 :=> 

data Qual t where
  (:=>) :: [Pred] -> t -> Qual t
   deriving Show

data Pred where
 IsIn :: String -> FType -> Pred
 Has :: FType -> String -> FType -> Pred
  deriving (Show, Eq)

data FType where
    FGen :: Int -> FType
    FInt :: FType
    FFloat :: FType
    FString :: FType
    FBool :: FType
    FList :: FType -> FType
    FVar :: Ident -> FType
    FRTy :: Ident -> FType -> FType
    FRec :: [(String, FType)] -> FType
    FFn :: FType -> FType -> FType
 deriving (Show, Eq, Ord)

int :: FType
int = FInt
float :: FType
float = FFloat
string :: FType
string = FString
bool ::  FType
bool = FBool
list :: FType -> FType
list t = FList t
var :: Ident -> FType
var i = FVar i
rec :: [(String, FType)] -> FType
rec s = FRec s
fn :: FType -> FType -> FType
fn t1 t2 = FFn t1 t2
genT :: Int -> FType
genT i = FGen i  

infixr 6 .->

(.->) :: FType -> FType -> FType 
t1 .-> t2 = fn t1 t2

class VarContainer a where
   ftv :: a -> S.Set Ident
   hasQVar :: a -> Bool
   
class HasType a where
  typeOf :: a -> Qual FType
  
mergeQuals :: [Pred] -> [Pred] -> [Pred]
mergeQuals []     t  = t
mergeQuals t      [] = t
mergeQuals (p:ps) t  = if L.elem p t then mergeQuals ps t else mergeQuals ps (p:t)

mergeQuals' :: [[Pred]] -> [Pred]
mergeQuals' = foldr mergeQuals []   