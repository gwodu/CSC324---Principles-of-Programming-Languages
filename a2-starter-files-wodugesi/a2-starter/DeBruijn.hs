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
removeNames ctx t = error "TODO"

shifting :: Int -> Int -> Term -> Term
shifting d c t = error "TODO"

subst :: Int -> Term -> Term -> Term
subst j s t = error "TODO"
