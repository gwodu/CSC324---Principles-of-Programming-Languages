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
substType s@(Subst m) ty = case ty of
  TyBool -> TyBool
  TyNat -> TyNat
  TyTop -> TyTop
  TyArrow t1 t2 -> TyArrow (substType s t1) (substType s t2)
  TyVar v -> case lookup v m of
               Just t -> t
               Nothing -> TyVar v
  TyRecord fields -> TyRecord [(l, substType s t) | (l, t) <- fields]
  TyProd t1 t2 -> TyProd (substType s t1) (substType s t2)
  TySum t1 t2 -> TySum (substType s t1) (substType s t2)

composeSubst :: Subst -> Subst -> Subst
composeSubst s1@(Subst m1) (Subst m2) =
  Subst (map (substType s1) m2 `union` m1)

{------------------------------------------------------
  Free type variables
------------------------------------------------------}

ftv :: Type -> [TypeVar]
ftv TyBool = []
ftv TyNat = []
ftv TyTop = []
ftv (TyVar v) = [v]
ftv (TyArrow t1 t2) = ftv t1 ++ ftv t2
ftv (TyRecord fields) = concatMap (ftv . snd) fields
ftv (TyProd t1 t2) = ftv t1 ++ ftv t2
ftv (TySum t1 t2) = ftv t1 ++ ftv t2
