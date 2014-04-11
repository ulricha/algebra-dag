-- This module provides functions to replace references within queries.
module Database.Algebra.SQL.Query.Substitution
    ( replaceReferencesSelectStmt
    ) where

import Control.Monad (liftM)

import qualified Database.Algebra.SQL.Query as Q

type SubstitutionFunction = Q.ReferenceType -> Q.FromExpr

-- | Replaces all references in this 'Q.SelectStmt' with the result from the
-- given function.
replaceReferencesSelectStmt :: SubstitutionFunction
                            -> Q.SelectStmt
                            -> Q.SelectStmt
replaceReferencesSelectStmt r body =
    body
    { Q.selectClause = map (replaceReferencesSelectColumn r)
                           $ Q.selectClause body
    , Q.fromClause = map (replaceReferencesFromPart r) $ Q.fromClause body
    , Q.whereClause = liftM (replaceReferencesColumnExpr r) $ Q.whereClause body
    , Q.groupByClause = map (replaceReferencesColumnExpr r)
                            $ Q.groupByClause body
    , Q.orderByClause = map (replaceReferencesOrderExpr r)
                            $ Q.orderByClause body
    }

replaceReferencesSelectColumn :: SubstitutionFunction
                              -> Q.SelectColumn
                              -> Q.SelectColumn
replaceReferencesSelectColumn r (Q.SCAlias e a)     =
    Q.SCAlias (replaceReferencesExtendedExpr r e) a

replaceReferencesExtendedExpr :: SubstitutionFunction
                              -> Q.ExtendedExpr
                              -> Q.ExtendedExpr
replaceReferencesExtendedExpr r (Q.EEBase v)        =
    Q.EEBase $ replaceReferencesValueExprTemplate replaceReferencesExtendedExpr
                                                  r
                                                  v
replaceReferencesExtendedExpr r (Q.EERowNum p o)    =
    Q.EERowNum (liftM (replaceReferencesAggrExpr r) p)
               (map (replaceReferencesOrderExpr r) o)

replaceReferencesExtendedExpr r (Q.EEDenseRank o)   =
    Q.EEDenseRank (map (replaceReferencesOrderExpr r) o)
replaceReferencesExtendedExpr r (Q.EERank o)        =
    Q.EERank (map (replaceReferencesOrderExpr r) o)
replaceReferencesExtendedExpr r (Q.EEAggrExpr ae) =
    Q.EEAggrExpr $ replaceReferencesAggrExpr r ae

-- | Generic value expression substitution function.
replaceReferencesValueExprTemplate :: (SubstitutionFunction -> a -> a)
                                   -> SubstitutionFunction
                                   -> Q.ValueExprTemplate a
                                   -> Q.ValueExprTemplate a
replaceReferencesValueExprTemplate replaceReferencesRec r ve = case ve of
    Q.VEExists q -> Q.VEExists $ replaceReferencesValueQuery r q
    Q.VEIn ae q  -> Q.VEIn (replaceReferencesRec r ae)
                           (replaceReferencesValueQuery r q)
    _            -> ve

replaceReferencesColumnExpr :: SubstitutionFunction
                            -> Q.ColumnExpr
                            -> Q.ColumnExpr
replaceReferencesColumnExpr r (Q.CEBase v) =
    Q.CEBase $ replaceReferencesValueExprTemplate replaceReferencesColumnExpr
                                                  r
                                                  v

replaceReferencesAggrExpr :: SubstitutionFunction
                          -> Q.AggrExpr
                          -> Q.AggrExpr
replaceReferencesAggrExpr r (Q.AEBase v)        =
    Q.AEBase $ replaceReferencesValueExprTemplate replaceReferencesAggrExpr
                                                  r
                                                  v
replaceReferencesAggrExpr r (Q.AEAggregate v f) =
    Q.AEAggregate (liftM (replaceReferencesColumnExpr r) v) f

replaceReferencesFromPart :: SubstitutionFunction
                          -> Q.FromPart
                          -> Q.FromPart
replaceReferencesFromPart r (Q.FPAlias e a c) =
    Q.FPAlias (replaceReferencesFromExpr r e) a c

replaceReferencesFromExpr :: SubstitutionFunction
                          -> Q.FromExpr
                          -> Q.FromExpr
replaceReferencesFromExpr r (Q.FESubQuery q) =
    Q.FESubQuery $ replaceReferencesValueQuery r q

replaceReferencesFromExpr r (Q.FEVariable v) = r v

replaceReferencesFromExpr _ t                = t

replaceReferencesValueQuery :: SubstitutionFunction
                            -> Q.ValueQuery
                            -> Q.ValueQuery
replaceReferencesValueQuery r (Q.VQSelect s) =
    Q.VQSelect $ replaceReferencesSelectStmt r s

replaceReferencesValueQuery r
    (Q.VQWith bindings body) =
    Q.VQWith (map f bindings) $ replaceReferencesValueQuery r body
  where f (n, oC, q) = (n, oC, replaceReferencesValueQuery r q)

replaceReferencesValueQuery r
    (Q.VQBinarySetOperation lq rq o) =
    Q.VQBinarySetOperation (replaceReferencesValueQuery r lq)
                           (replaceReferencesValueQuery r rq)
                           o

replaceReferencesValueQuery _ q = q

replaceReferencesOrderExpr :: SubstitutionFunction
                           -> Q.OrderExpr
                           -> Q.OrderExpr
replaceReferencesOrderExpr r (Q.OE ae d) =
    Q.OE (replaceReferencesAggrExpr r ae) d

