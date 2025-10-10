module DeBruijn where

import Term

type NamingContext = [Identifier]

-- indexOf
-- This function finds the first occurrence of an identifier in a naming
-- context and returns the index it is found at
indexOf :: Identifier -> NamingContext -> Int
indexOf _    []             = error "Non-existing variable"
indexOf iden (iden' : ctx') =
  if iden == iden'
  then 0
  else 1 + indexOf iden ctx'

removeNames :: NamingContext -> NamedTerm -> Term
removeNames ctx NTTrue = TmTrue
removeNames ctx NTFalse = TmFalse
removeNames ctx (NTIf t1 t2 t3) =
  TmIf (removeNames ctx t1) (removeNames ctx t2) (removeNames ctx t3)
removeNames ctx NTZero = TmZero
removeNames ctx (NTSucc t) = TmSucc (removeNames ctx t)
removeNames ctx (NTPred t) = TmPred (removeNames ctx t)
removeNames ctx (NTIsZero t) = TmIsZero (removeNames ctx t)
removeNames ctx (NTVar iden) = TmVar (indexOf iden ctx)
removeNames ctx (NTAbs iden body) = TmAbs (removeNames (iden : ctx) body)
removeNames ctx (NTApp t1 t2) = TmApp (removeNames ctx t1) (removeNames ctx t2)

shifting :: Int -> Int -> Term -> Term
shifting d c TmTrue = TmTrue
shifting d c TmFalse = TmFalse
shifting d c (TmIf t1 t2 t3) =
  TmIf (shifting d c t1) (shifting d c t2) (shifting d c t3)
shifting d c TmZero = TmZero
shifting d c (TmSucc t) = TmSucc (shifting d c t)
shifting d c (TmPred t) = TmPred (shifting d c t)
shifting d c (TmIsZero t) = TmIsZero (shifting d c t)
shifting d c (TmVar k) = 
  if k >= c
  then TmVar (k + d)
  else TmVar k
shifting d c (TmAbs body) = TmAbs (shifting d (c + 1) body)
shifting d c (TmApp t1 t2) = TmApp (shifting d c t1) (shifting d c t2)

subst :: Int -> Term -> Term -> Term
subst j s TmTrue = TmTrue
subst j s TmFalse = TmFalse
subst j s (TmIf t1 t2 t3) = 
  TmIf (subst j s t1) (subst j s t2) (subst j s t3)
subst j s TmZero = TmZero
subst j s (TmSucc t) = TmSucc (subst j s t)
subst j s (TmPred t) = TmPred (subst j s t)
subst j s (TmIsZero t) = TmIsZero (subst j s t)
subst j s (TmVar k) = 
  if k == j
  then s
  else TmVar k
subst j s (TmAbs body) = TmAbs (subst (j + 1) (shifting 1 0 s) body)
subst j s (TmApp t1 t2) = TmApp (subst j s t1) (subst j s t2)
