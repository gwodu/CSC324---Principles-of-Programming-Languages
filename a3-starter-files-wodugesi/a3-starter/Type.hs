module Type where

data Type = TyBool                    -- Base type: Bool
          | TyNat                     -- Base type: Nat
          | TyArrow Type Type         -- Function type: T₁ → T₂
          | TyRecord [(String, Type)] -- Record type: { lᵢ : Tᵢ }
          | TyProd Type Type          -- Product type: T₁ × T₂
          | TySum Type Type           -- Sum type: T₁ + T₂
          deriving (Eq, Show)
