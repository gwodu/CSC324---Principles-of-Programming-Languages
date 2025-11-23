module Infer where

import Term
import Type
import InferMonad
import Unify

type InferContext = [Type]

lookupCtx :: Int -> InferContext -> Maybe Type
lookupCtx k ctx = if k < 0 || k >= length ctx
                then Nothing
                else Just (ctx !! k)

infer :: InferContext -> Term -> InferM (Type, Constraints)
infer ctx TmTrue = return (TyBool, [])
infer ctx TmFalse = return (TyBool, [])
infer ctx (TmIf t1 t2 t3) = do
  (ty1, cs1) <- infer ctx t1
  (ty2, cs2) <- infer ctx t2
  (ty3, cs3) <- infer ctx t3
  return (ty2, cs1 ++ cs2 ++ cs3 ++ [CEq ty1 TyBool, CEq ty2 ty3])
infer ctx TmZero = return (TyNat, [])
infer ctx (TmSucc t) = do
  (ty, cs) <- infer ctx t
  return (TyNat, cs ++ [CEq ty TyNat])
infer ctx (TmPred t) = do
  (ty, cs) <- infer ctx t
  return (TyNat, cs ++ [CEq ty TyNat])
infer ctx (TmIsZero t) = do
  (ty, cs) <- infer ctx t
  return (TyBool, cs ++ [CEq ty TyNat])
infer ctx (TmVar k) =
  case lookupCtx k ctx of
    Nothing -> inferFail $ "Variable " ++ show k ++ " not found in context"
    Just ty -> return (ty, [])
infer ctx (TmApp t1 t2) = do
  (ty1, cs1) <- infer ctx t1
  (ty2, cs2) <- infer ctx t2
  tyX <- TyVar <$> freshTypeVar
  return (tyX, cs1 ++ cs2 ++ [CEq ty1 (TyArrow ty2 tyX)])
infer ctx (TmAbs t) = do
  tyX <- TyVar <$> freshTypeVar
  (tyBody, cs) <- infer (tyX : ctx) t
  return (TyArrow tyX tyBody, cs)
infer ctx (TmAnnAbs ty t) = do
  (tyBody, cs) <- infer (ty : ctx) t
  return (TyArrow ty tyBody, cs)
infer _ t = inferFail $ "Unsupported term (not required)"

inferType :: Term -> Either String Type
inferType t =
  case runInferM (infer [] t) of
    InferOk (ty, cs) _ ->
      case unifyConstraints cs of
        UnifyOk subst -> Right $ substType subst ty
        UnifyError err -> Left err
    InferError err -> Left err
