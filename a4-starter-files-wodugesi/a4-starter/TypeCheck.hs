module TypeCheck where

import Term
import Type

{--------------------------------------------------------
  Subtyping
--------------------------------------------------------}

(<:) :: Type -> Type -> Bool
(<:) = error "TODO"

{--------------------------------------------------------
  Type checking
  TODO Part 3: Add support for subtyping to typeCheck
--------------------------------------------------------}

data TcResult
  = TcOk Type
  | TcError String
  deriving (Show, Eq)

type TcContext = [Type]

lookupCtx :: Int -> TcContext -> Maybe Type
lookupCtx k ctx = if k < 0 || k >= length ctx
                then Nothing
                else Just (ctx !! k)

typeCheckFields :: TcContext -> [(String, Term)] -> Either String [(String, Type)]
typeCheckFields ctx xs = checkFields xs []
  where
    checkFields [] acc = Right (reverse acc)
    checkFields ((l, t) : ts) acc =
      case typeCheck ctx t of
        TcOk ty -> checkFields ts ((l, ty) : acc)
        TcError s -> Left s

typeCheck :: TcContext -> Term -> TcResult
typeCheck _ TmTrue = TcOk TyBool
typeCheck _ TmFalse = TcOk TyBool
typeCheck _ TmZero = TcOk TyNat

typeCheck ctx (TmIf t1 t2 t3) =
  case typeCheck ctx t1 of
    TcOk TyBool ->
      case (typeCheck ctx t2, typeCheck ctx t3) of 
        (TcOk ty2, TcOk ty3)
          | ty2 == ty3 -> TcOk ty2
          | otherwise -> TcError "If branches have different types"
        (err@TcError{}, _) -> err
        (_, err@TcError{}) -> err
    TcOk ty -> TcError $ "If condition must be Bool, but got " ++ show ty
    err -> err

typeCheck ctx (TmSucc t) = case typeCheck ctx t of
  TcOk TyNat -> TcOk TyNat
  TcOk ty -> TcError $ "Succ requires a Nat, but got " ++ show ty
  err -> err
typeCheck ctx (TmPred t) = case typeCheck ctx t of
  TcOk TyNat -> TcOk TyNat
  TcOk ty -> TcError $ "Pred requires a Nat, but got " ++ show ty
  err -> err

typeCheck ctx (TmIsZero t) =
  case typeCheck ctx t of
    TcOk TyNat -> TcOk TyBool
    TcOk ty -> TcError $ "IsZero requires a Nat, but got " ++ show ty
    err -> err

typeCheck ctx (TmVar x) =
  maybe (TcError "Variable not found in context") TcOk (lookupCtx x ctx)
  
typeCheck ctx (TmAnnAbs ty1 t) =
  case typeCheck (ty1 : ctx) t of
    TcOk ty2 -> TcOk (TyArrow ty1 ty2)
    err -> err

typeCheck ctx (TmApp t1 t2) =
  case typeCheck ctx t1 of
    TcOk (TyArrow ty11 ty12) ->
      case typeCheck ctx t2 of
        TcOk ty2 | ty2 == ty11 -> TcOk ty12
        TcOk ty2 -> TcError $ "Application requires the argument to be " ++ show ty11 ++ ", but got " ++ show ty2
        err -> err
    TcOk ty -> TcError $ "Application requires a function, but got " ++ show ty
    err -> err

typeCheck ctx (TmRecord fields) =
  either TcError (TcOk . TyRecord) (typeCheckFields ctx fields)

typeCheck ctx (TmProj t l) =
  case typeCheck ctx t of
    TcOk (TyRecord fields) ->
      maybe (TcError $ "Label " ++ l ++ " not found in record") TcOk (lookup l fields)
    TcOk ty -> TcError $ "Projection requires a record, but got " ++ show ty
    err -> err

typeCheck ctx (TmPair t1 t2) =
  case (typeCheck ctx t1, typeCheck ctx t2) of
    (TcOk ty1, TcOk ty2) -> TcOk (TyProd ty1 ty2)
    (err@TcError{}, _) -> err
    (_, err@TcError{}) -> err

typeCheck ctx (TmFst t) =
  case typeCheck ctx t of
    TcOk (TyProd ty1 _) -> TcOk ty1
    TcOk ty -> TcError $ "fst requires a product type, but got " ++ show ty
    err -> err

typeCheck ctx (TmSnd t) =
  case typeCheck ctx t of
    TcOk (TyProd _ ty2) -> TcOk ty2
    TcOk ty -> TcError $ "snd requires a product type, but got " ++ show ty
    err -> err

typeCheck ctx (TmInl t ty) =
  case ty of
    TySum ty1 _ ->
      case typeCheck ctx t of
        TcOk ty1' | ty1' == ty1 -> TcOk ty
        TcOk ty1' -> TcError $ "inl requires the argument to be " ++ show ty1 ++ ", but got " ++ show ty1'
        err -> err
    _ -> TcError "inl requires a sum type annotation"

typeCheck ctx (TmInr t ty) =
  case ty of
    TySum _ ty2 ->
      case typeCheck ctx t of
        TcOk ty2' | ty2' == ty2 -> TcOk ty
        TcOk ty2' -> TcError $ "inr requires the argument to be " ++ show ty2 ++ ", but got " ++ show ty2'
        err -> err
    _ -> TcError "inr requires a sum type annotation"

typeCheck ctx (TmMatch t t1 t2) =
  case typeCheck ctx t of
    TcOk (TySum tyL tyR) ->
      case (typeCheck (tyL : ctx) t1, typeCheck (tyR : ctx) t2) of
        (TcOk ty1, TcOk ty2)
          | ty1 == ty2 -> TcOk ty1
          | otherwise -> TcError "Match branches have different types"
        (err@TcError{}, _) -> err
        (_, err@TcError{}) -> err
    TcOk ty -> TcError $ "Match requires a sum type, but got " ++ show ty
    err -> err

typeCheck ctx (TmAbs t) = error "Unsupported term (not required)"

