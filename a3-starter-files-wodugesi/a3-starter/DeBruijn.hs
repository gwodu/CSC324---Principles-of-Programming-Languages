module DeBruijn where

import Data.List
import Term
import Type

shifting :: Int -> Int -> Term -> Term
shifting d c (TmIf t t1 t2) = TmIf (shifting d c t) (shifting d c t1) (shifting d c t2)
shifting d c (TmSucc t) = TmSucc (shifting d c t)
shifting d c (TmPred t) = TmPred (shifting d c t)
shifting d c (TmIsZero t) = TmIsZero (shifting d c t)
shifting d c (TmVar k) = if k >= c
                         then TmVar (k + d)
                         else TmVar k
shifting d c (TmAnnAbs ty t) = TmAnnAbs ty (shifting d (c + 1) t)
shifting d c (TmApp t1 t2) = TmApp (shifting d c t1) (shifting d c t2)
shifting d c (TmRecord ts) = TmRecord (map (\(l, t) -> (l, shifting d c t)) ts)
shifting d c (TmProj t id) = TmProj (shifting d c t) id
shifting d c (TmPair t1 t2) = TmPair (shifting d c t1) (shifting d c t2)
shifting d c (TmFst t) = TmFst (shifting d c t)
shifting d c (TmSnd t) = TmSnd (shifting d c t)
shifting d c (TmInl t ty) = TmInl (shifting d c t) ty
shifting d c (TmInr t ty) = TmInr (shifting d c t) ty
shifting d c (TmMatch t t1 t2) = TmMatch (shifting d c t) (shifting d (c + 1) t1) (shifting d (c + 1) t2)
-- The following are constant cases
shifting _ _ t = t

subst :: Int -> Term -> Term -> Term
subst j s (TmIf t t1 t2) = TmIf (subst j s t) (subst j s t1) (subst j s t2)
subst j s (TmSucc t) = TmSucc (subst j s t)
subst j s (TmPred t) = TmPred (subst j s t)
subst j s (TmIsZero t) = TmIsZero (subst j s t)
subst j s (TmVar k) = if k == j
                      then s
                      else TmVar k
subst j s (TmAnnAbs ty t) = TmAnnAbs ty (subst (j + 1) (shifting 1 0 s) t)
subst j s (TmApp t1 t2) = TmApp (subst j s t1) (subst j s t2)
subst j s (TmRecord ts) = TmRecord (map (\(l, t) -> (l, subst j s t)) ts)
subst j s (TmProj t id) = TmProj (subst j s t) id
subst j s (TmPair t1 t2) = TmPair (subst j s t1) (subst j s t2)
subst j s (TmFst t) = TmFst (subst j s t)
subst j s (TmSnd t) = TmSnd (subst j s t)
subst j s (TmInl t ty) = TmInl (subst j s t) ty
subst j s (TmInr t ty) = TmInr (subst j s t) ty
subst j s (TmMatch t t1 t2) = TmMatch (subst j s t) (subst (j + 1) (shifting 1 0 s) t1) (subst (j + 1) (shifting 1 0 s) t2)
-- The following are constant cases
subst _ _ t = t

