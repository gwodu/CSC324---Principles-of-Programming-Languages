module Term where

import Type

data Term = TmTrue                    -- true
          | TmFalse                   -- false
          | TmIf Term Term Term       -- if t1 then t2 else t3
          | TmZero                    -- 0
          | TmSucc Term               -- succ t
          | TmPred Term               -- pred t
          | TmIsZero Term             -- iszero t
          | TmVar Int                 -- DeBruijn index k
          | TmAbs Term                -- λ t (unannotated lambda; for part 2 only)
          | TmApp Term Term           -- (t1 t2)
          | TmAnnAbs Type Term        -- λ T . t  (annotated lambda)
          -- Part 3 only below this line
          | TmRecord [(String, Term)] -- { lᵢ = tᵢ }
          | TmProj Term String        -- t.l
          | TmPair Term Term          -- { t₁, t₂ }
          | TmFst Term                -- t.1
          | TmSnd Term                -- t.2
          | TmInl Term Type           -- inl t as T
          | TmInr Term Type           -- inr t as T
          | TmMatch Term Term Term    -- case t of inl x ⇒ t₁ | inr x ⇒ t₂
          deriving (Eq, Show)
