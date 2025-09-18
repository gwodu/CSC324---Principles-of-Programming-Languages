module Term where

data Term = TmTrue              -- true
          | TmFalse             -- false
          | TmIf Term Term Term -- if t1 then t2 else t3
          | TmZero              -- 0
          | TmSucc Term         -- succ t
          | TmPred Term         -- pred t
          | TmIsZero Term       -- iszero t
          deriving (Eq, Show)

data Value = VTrue              -- true
           | VFalse             -- false
           | VNum NumValue      -- nv
           deriving (Eq, Show)

data NumValue = VZero           -- 0
              | VSucc NumValue  -- succ nv
              deriving (Eq, Show)
