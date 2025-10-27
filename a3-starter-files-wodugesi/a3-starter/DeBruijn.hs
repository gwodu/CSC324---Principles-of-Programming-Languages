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
shifting d c (TmRecord ts) = error "TODO"
shifting d c (TmProj t id) = error "TODO"
shifting d c (TmPair t1 t2) = error "TODO"
shifting d c (TmFst t) = error "TODO"
shifting d c (TmSnd t) = error "TODO"
shifting d c (TmInl t ty) = error "TODO"
shifting d c (TmInr t ty) = error "TODO"
shifting d c (TmMatch t t1 t2) = error "TODO"
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
subst j s (TmRecord ts) = error "TODO"
subst j s (TmProj t id) = error "TODO"
subst j s (TmPair t1 t2) = error "TODO"
subst j s (TmFst t) = error "TODO"
subst j s (TmSnd t) = error "TODO"
subst j s (TmInl t ty) = error "TODO"
subst j s (TmInr t ty) = error "TODO"
subst j s (TmMatch t t1 t2) = error "TODO"
-- The following are constant cases
subst _ _ t = t

