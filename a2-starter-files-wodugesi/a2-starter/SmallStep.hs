module SmallStep where

import Term
import DeBruijn

type ErrorMessage = String

data StepResult
  = StepOk Term
  | StepError ErrorMessage
  deriving (Eq, Show)

isVal :: Term -> Bool
isVal t = isBoolVal t
          || isNumVal t
          || isLambdaForm t

isBoolVal :: Term -> Bool
isBoolVal TmTrue = True
isBoolVal TmFalse = True
isBoolVal _ = False

isNumVal :: Term -> Bool
isNumVal TmZero = True
isNumVal (TmSucc t) = isNumVal t
isNumVal _ = False

isLambdaForm :: Term -> Bool
isLambdaForm (TmAbs _) = True
isLambdaForm _ = False

oneStep :: Term -> StepResult
oneStep (TmIf TmTrue t1 _) = StepOk t1                   -- E-IfTrue
oneStep (TmIf TmFalse _ t2) = StepOk t2                  -- E-IfFalse
oneStep (TmIf t t1 t2) =                                 -- E-If
  case oneStep t of
    StepOk t' -> StepOk (TmIf t' t1 t2)
    StepError msg -> StepError ("The condition cannot be stepped: "
                                ++ show t ++ "\n"
                                ++ msg)
oneStep (TmSucc t) =                                     -- E-Succ
  case oneStep t of
    StepOk t' -> StepOk (TmSucc t')
    StepError msg -> StepError ("The argument cannot be stepped: "
                                ++ show t ++ "\n"
                                ++ msg)
oneStep (TmPred TmZero) = StepOk TmZero                  -- E-PredZero
oneStep (TmPred (TmSucc t))                              -- E-PredSucc
  | isNumVal t = StepOk t
oneStep (TmPred t) =                                     -- E-Pred
  case oneStep t of
    StepOk t' -> StepOk (TmPred t')
    StepError msg -> StepError ("The argument cannot be stepped: "
                                ++ show t ++ "\n"
                                ++ msg)
oneStep (TmIsZero TmZero) = StepOk TmTrue                -- E-IsZeroZero
oneStep (TmIsZero (TmSucc t))                            -- E-IsZeroSucc
  | isNumVal t = StepOk TmFalse
oneStep (TmIsZero t) =                                   -- E-IsZero
  case oneStep t of
    StepOk t' -> StepOk (TmIsZero t')
    StepError msg -> StepError ("The argument cannot be stepped: "
                                ++ show t ++ "\n"
                                ++ msg)

-- E-AppAbs
oneStep (TmApp (TmAbs t12) v2)
  | isVal v2 = StepOk (shifting (-1) 0 (subst 0 (shifting 1 0 v2) t12))

-- E-App2
oneStep (TmApp v1 t2)
  | isVal v1 = case oneStep t2 of
                  StepOk t2' -> StepOk (TmApp v1 t2')
                  StepError msg -> StepError ("The argument cannot be stepped: " ++ show t2 ++ "\n" ++ msg)

-- E-app1
oneStep (TmApp t1 t2) =
  case oneStep t1 of
    StepOk t1' -> StepOk (TmApp t1' t2)
    StepError msg -> StepError ("The function cannot be stepped: "
                                ++ show t1 ++ "\n"
                                ++ msg)

oneStep t = StepError ("No applicable evaluation rule for: " ++ show t)

multiStep :: Term -> Term
multiStep t = if isVal t
              then t
              else case oneStep t of
                     StepOk t' -> multiStep t'
                     StepError _ -> t
