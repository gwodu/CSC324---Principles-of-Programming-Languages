module Type where

import Prelude hiding (lookup, map)
import Data.Map (Map, empty, union, map, lookup, singleton)

{--------------------------------------------------------
  Type and constraint syntax (PLEASE DO NOT TOUCH)
--------------------------------------------------------}

data Type = TyBool                    -- Base type: Bool
          | TyNat                     -- Base type: Nat
          | TyArrow Type Type         -- Function type: T₁ → T₂
          | TyVar TypeVar             -- Type variable             (for part 2 only)
          | TyRecord [(String, Type)] -- Record type: { lᵢ : Tᵢ }  (for part 3 only)
          | TyProd Type Type          -- Product type: T₁ × T₂     (for part 3 only)
          | TySum Type Type           -- Sum type: T₁ + T₂         (for part 3 only)
          | TyTop                     -- Top type: Top             (for part 3 only)
          deriving (Eq, Show)

data TypeVar = TypeVar Int
  deriving (Eq, Show, Ord)

data Constraint = CEq Type Type
  deriving (Show)

type Constraints = [Constraint]

{--------------------------------------------------------
  Substitutions
--------------------------------------------------------}

data Subst = Subst (Map TypeVar Type)
  deriving (Eq, Show)

emptySubst :: Subst
emptySubst = Subst empty

substType :: Subst -> Type -> Type
substType s ty = error "TODO"

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = error "TODO"

{------------------------------------------------------
  Free type variables
------------------------------------------------------}

ftv :: Type -> [TypeVar]
ftv ty = error "TODO"
