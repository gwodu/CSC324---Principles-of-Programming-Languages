module TypeCheck where

import Term
import Type

data TcResult
  = TcOk Type
  | TcError String
  deriving (Show, Eq)

type TcContext = [Type]

lookupCtx :: Int -> TcContext -> Maybe Type
lookupCtx k ctx = if k < 0 || k >= length ctx
                then Nothing
                else Just (ctx !! k)

typeCheck :: TcContext -> Term -> TcResult

-- Booleans
typeCheck _ TmTrue = TcOk TyBool
typeCheck _ TmFalse = TcOk TyBool

-- Naturals
typeCheck _ TmZero = TcOk TyNat
typeCheck ctx (TmSucc t) = case typeCheck ctx t of
  TcOk TyNat -> TcOk TyNat
  _ -> TcError "succ expects Nat"
typeCheck ctx (TmPred t) = case typeCheck ctx t of
  TcOk TyNat -> TcOk TyNat
  _ -> TcError "pred expects Nat"
typeCheck ctx (TmIsZero t) = case typeCheck ctx t of
  TcOk TyNat -> TcOk TyBool
  _ -> TcError "iszero expects Nat"

-- Conditionals
typeCheck ctx (TmIf cond thenBranch elseBranch) = case typeCheck ctx cond of
  TcOk TyBool -> case typeCheck ctx thenBranch of
    TcOk t1 -> case typeCheck ctx elseBranch of
      TcOk t2 | t1 == t2 -> TcOk t1
      TcOk _ -> TcError "branches must have same type"
      err -> err
    err -> err
  _ -> TcError "condition must be Bool"

-- Variables
typeCheck ctx (TmVar k) = case lookupCtx k ctx of
  Just ty -> TcOk ty
  Nothing -> TcError ("variable " ++ show k ++ " not in context")

-- Lambda abstraction
typeCheck ctx (TmAnnAbs paramType body) = case typeCheck (paramType : ctx) body of
  TcOk bodyType -> TcOk (TyArrow paramType bodyType)
  err -> err

-- Application
typeCheck ctx (TmApp func arg) = case typeCheck ctx func of
  TcOk (TyArrow argType resultType) -> case typeCheck ctx arg of
    TcOk argType' | argType == argType' -> TcOk resultType
    TcOk _ -> TcError "argument type mismatch"
    err -> err
  TcOk _ -> TcError "expected function type"
  err -> err

-- Records
typeCheck ctx (TmRecord fields) = 
  let checkField (label, term) = 
        case typeCheck ctx term of
          TcOk ty -> Just (label, ty)
          _ -> Nothing
  in case mapM checkField fields of
       Just fieldTypes -> TcOk (TyRecord fieldTypes)
       Nothing -> TcError "all fields must be well-typed"

-- Record projection
typeCheck ctx (TmProj record label) = case typeCheck ctx record of
  TcOk (TyRecord fields) -> case lookup label fields of
    Just ty -> TcOk ty
    Nothing -> TcError ("label " ++ label ++ " not found")
  _ -> TcError "projection expects record type"

-- Pairs
typeCheck ctx (TmPair t1 t2) = case (typeCheck ctx t1, typeCheck ctx t2) of
  (TcOk ty1, TcOk ty2) -> TcOk (TyProd ty1 ty2)
  (TcError msg, _) -> TcError msg
  (_, TcError msg) -> TcError msg

-- Pair projections
typeCheck ctx (TmFst pair) = case typeCheck ctx pair of
  TcOk (TyProd ty1 _) -> TcOk ty1
  TcOk _ -> TcError "fst expects product type"
  err -> err

typeCheck ctx (TmSnd pair) = case typeCheck ctx pair of
  TcOk (TyProd _ ty2) -> TcOk ty2
  TcOk _ -> TcError "snd expects product type"
  err -> err

-- Sums
typeCheck ctx (TmInl term sumType) = case sumType of
  TySum leftType _ -> case typeCheck ctx term of
    TcOk termType | termType == leftType -> TcOk sumType
    _ -> TcError "inl argument type mismatch"
  _ -> TcError "inl expects sum type"

typeCheck ctx (TmInr term sumType) = case sumType of
  TySum _ rightType -> case typeCheck ctx term of
    TcOk termType | termType == rightType -> TcOk sumType
    _ -> TcError "inr argument type mismatch"
  _ -> TcError "inr expects sum type"

typeCheck ctx (TmMatch sumTerm leftBranch rightBranch) = case typeCheck ctx sumTerm of
  TcOk (TySum leftType rightType) -> case typeCheck (leftType : ctx) leftBranch of
    TcOk t1 -> case typeCheck (rightType : ctx) rightBranch of
      TcOk t2 | t1 == t2 -> TcOk t1
      TcOk _ -> TcError "match branches must have same type"
      err -> err
    err -> err
  _ -> TcError "match expects sum type"
