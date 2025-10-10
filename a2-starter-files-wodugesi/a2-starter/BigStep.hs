module BigStep where

import Term

type ErrorMessage = String

data EvalResult
  = EvalOk Value
  | EvalError ErrorMessage
  deriving (Eq, Show)

-- B-value
bigStep :: Env -> NamedTerm -> EvalResult
bigStep env NTTrue = EvalOk VTrue
bigStep env NTFalse = EvalOk VFalse

-- B-IfTrue / B-IfFalse
bigStep env (NTIf t1 t2 t3) = case bigStep env t1 of
  EvalOk VTrue -> bigStep env t2
  EvalOk VFalse -> bigStep env t3
  EvalOk (VNum _) -> EvalError "Type error: Condition must be a boolean"
  EvalOk (Closure _ _ _) -> EvalError "Type error: Condition must be a boolean"
  EvalError msg -> EvalError msg

-- B-value, for 0
bigStep env NTZero = EvalOk (VNum VZero)

-- B-Succ
bigStep env (NTSucc t1) = case bigStep env t1 of
  EvalOk (VNum nv1) -> EvalOk (VNum (VSucc nv1))
  EvalOk v -> EvalError ("Expected numeric value in succ, got: " ++ show v )
  EvalError msg -> EvalError ("Error in succ argument: " ++ msg)

bigStep env (NTPred t1) =
  case bigStep env t1 of
    EvalOk (VNum VZero) -> EvalOk (VNum VZero)
    EvalOk (VNum (VSucc nv1)) -> EvalOk (VNum nv1)
    EvalOk v -> EvalError ("Expected numeric value in pred, got: " ++ show v)
    EvalError msg -> EvalError ("Error in pred argument: " ++ msg)

bigStep env (NTIsZero t1) = case bigStep env t1 of
  EvalOk (VNum VZero) -> EvalOk VTrue
  EvalOk (VNum (VSucc _)) -> EvalOk VFalse
  EvalOk v -> EvalError ("Expected numeric value in iszero, got: " ++ show v)
  EvalError msg -> EvalError ("Error in iszero argument: " ++ msg)

-- B-Var
bigStep env (NTVar x) =
  case lookup x env of
    Just v -> EvalOk v
    Nothing -> EvalError ("Unbound variable: " ++ x)

-- B-Abs
bigStep env (NTAbs x body) = EvalOk (Closure env x body)

bigStep env (NTApp t1 t2) = case bigStep env t1 of 
  EvalOk (Closure env1 x body) -> case bigStep env t2 of 
    EvalOk v2 -> bigStep ((x, v2) : env1) body
    EvalError msg -> EvalError ("Error evaluating argument: " ++ msg)
  EvalOk v -> EvalError ("Expected function in application, got: " ++ show v)
  EvalError msg -> EvalError ("Error evaluating function: " ++ msg)
