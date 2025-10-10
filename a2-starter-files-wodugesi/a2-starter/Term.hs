module Term where

type Identifier = String

type Env = [(Identifier, Value)]

data NamedTerm = NTTrue                             -- true
               | NTFalse                            -- false
               | NTIf NamedTerm NamedTerm NamedTerm -- if t1 then t2 else t3
               | NTZero                             -- 0
               | NTSucc NamedTerm                   -- succ t
               | NTPred NamedTerm                   -- pred t
               | NTIsZero NamedTerm                 -- iszero t
               | NTVar Identifier                   -- variable name "x"
               | NTAbs Identifier NamedTerm         -- λ x . t
               | NTApp NamedTerm NamedTerm          -- (t1 t2)
               deriving (Eq, Show)

data Value = VTrue                            -- true
           | VFalse                           -- false
           | VNum NumValue                    -- nv
           | Closure Env Identifier NamedTerm -- closure
           deriving (Eq, Show)

data NumValue = VZero          -- 0
              | VSucc NumValue -- succ nv
              deriving (Eq, Show)

data Term = TmTrue              -- true
          | TmFalse             -- false
          | TmIf Term Term Term -- if t1 then t2 else t3
          | TmZero              -- 0
          | TmSucc Term         -- succ t
          | TmPred Term         -- pred t
          | TmIsZero Term       -- iszero t
          | TmVar Int           -- DeBruijn index k
          | TmAbs Term          -- λ . t
          | TmApp Term Term     -- (t1 t2)
          deriving (Eq, Show)
