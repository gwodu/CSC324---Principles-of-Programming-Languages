module SmallStep where

import Term

type ErrorMessage = String

data StepResult
  = StepOk Term
  | StepError ErrorMessage
  deriving (Eq, Show)

isVal :: Term -> Bool
isVal t = isBoolVal t
          || isNumVal t

isBoolVal :: Term -> Bool
isBoolVal TmTrue = True
isBoolVal TmFalse = True
isBoolVal _ = False

isNumVal :: Term -> Bool
isNumVal TmZero = True
isNumVal (TmSucc t) = isNumVal t
isNumVal _ = False

-- oneStep :: Term -> StepResult
-- oneStep _ = error "TODO"

oneStep :: Term -> StepResult
-- Values cannot step
oneStep t | isVal t = StepError ("Value " ++ show t ++ " cannot step")

-- E-IFTRUE: if true then t2 else t3 → t2
oneStep (TmIf TmTrue t2 _) = StepOk t2

-- E-IFFALSE: if false then t2 else t3 → t3
oneStep (TmIf TmFalse _ t3) = StepOk t3

-- E-IF: t1 → t1' implies if t1 then t2 else t3 → if t1' then t2 else t3
oneStep (TmIf t1 t2 t3) = 
  case oneStep t1 of
    StepOk t1' -> StepOk (TmIf t1' t2 t3)
    StepError msg -> StepError ("In if condition: " ++ msg)

-- E-SUCC: t1 → t1' implies succ t1 → succ t1'
oneStep (TmSucc t1) = 
  case oneStep t1 of
    StepOk t1' -> StepOk (TmSucc t1')
    StepError msg -> StepError ("In succ argument: " ++ msg)

-- E-PREDZERO: pred 0 → 0
oneStep (TmPred TmZero) = StepOk TmZero

-- E-PREDSUCC: pred (succ nv1) → nv1 (only when nv1 is a numeric value)
oneStep (TmPred (TmSucc nv1)) | isNumVal nv1 = StepOk nv1

-- E-PRED: t1 → t1' implies pred t1 → pred t1'
oneStep (TmPred t1) = 
  case oneStep t1 of
    StepOk t1' -> StepOk (TmPred t1')
    StepError msg -> StepError ("In pred argument: " ++ msg)

-- E-ISZEROZERO: iszero 0 → true
oneStep (TmIsZero TmZero) = StepOk TmTrue

-- E-ISZEROSUCC: iszero (succ nv1) → false (only when nv1 is a numeric value)
oneStep (TmIsZero (TmSucc nv1)) | isNumVal nv1 = StepOk TmFalse

-- E-ISZERO: t1 → t1' implies iszero t1 → iszero t1'
oneStep (TmIsZero t1) = 
  case oneStep t1 of
    StepOk t1' -> StepOk (TmIsZero t1')
    StepError msg -> StepError ("In iszero argument: " ++ msg)

-- Catch-all for any terms that don't match above patterns
oneStep t = StepError ("No evaluation rule applies to term: " ++ show t)

multiStep :: Term -> Term
multiStep t = if isVal t
              then t
              else case oneStep t of
                     StepOk t' -> multiStep t'
                     StepError _ -> t
