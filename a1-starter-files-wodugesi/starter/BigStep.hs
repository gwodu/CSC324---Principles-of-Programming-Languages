module BigStep where

import Term

type ErrorMessage = String

data EvalResult
  = EvalOk Value
  | EvalError ErrorMessage
  deriving (Eq, Show)

-- bigStep :: Term -> EvalResult
-- bigStep _ = error "TODO"

bigStep :: Term -> EvalResult
-- B-VALUE: Values evaluate to themselves
bigStep TmTrue = EvalOk VTrue
bigStep TmFalse = EvalOk VFalse
bigStep TmZero = EvalOk (VNum VZero)

-- B-SUCC: succ t1 ⇓ succ v1 if t1 ⇓ v1 and v1 is numeric
bigStep (TmSucc t1) = 
  case bigStep t1 of
    EvalOk (VNum nv1) -> EvalOk (VNum (VSucc nv1))
    EvalOk v -> EvalError ("succ applied to non-numeric value: " ++ show v)
    EvalError msg -> EvalError ("In succ argument: " ++ msg)

-- B-PREDZERO: pred t1 ⇓ 0 if t1 ⇓ 0
-- B-PREDSUCC: pred t1 ⇓ v if t1 ⇓ succ v and v is numeric
bigStep (TmPred t1) = 
  case bigStep t1 of
    EvalOk (VNum VZero) -> EvalOk (VNum VZero)
    EvalOk (VNum (VSucc nv)) -> EvalOk (VNum nv)
    EvalOk v -> EvalError ("pred applied to non-numeric value: " ++ show v)
    EvalError msg -> EvalError ("In pred argument: " ++ msg)

-- B-ISZEROZERO: iszero t1 ⇓ true if t1 ⇓ 0
-- B-ISZEROSUCC: iszero t1 ⇓ false if t1 ⇓ succ v and v is numeric
bigStep (TmIsZero t1) = 
  case bigStep t1 of
    EvalOk (VNum VZero) -> EvalOk VTrue
    EvalOk (VNum (VSucc _)) -> EvalOk VFalse
    EvalOk v -> EvalError ("iszero applied to non-numeric value: " ++ show v)
    EvalError msg -> EvalError ("In iszero argument: " ++ msg)

-- B-IFTRUE: if t1 then t2 else t3 ⇓ v2 if t1 ⇓ true and t2 ⇓ v2
-- B-IFFALSE: if t1 then t2 else t3 ⇓ v3 if t1 ⇓ false and t3 ⇓ v3
bigStep (TmIf t1 t2 t3) = 
  case bigStep t1 of
    EvalOk VTrue -> bigStep t2
    EvalOk VFalse -> bigStep t3
    EvalOk v -> EvalError ("if condition evaluated to non-boolean value: " ++ show v)
    EvalError msg -> EvalError ("In if condition: " ++ msg)