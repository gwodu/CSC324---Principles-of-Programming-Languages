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
infer ctx TmTrue = error "TODO"
infer ctx TmFalse = error "TODO"
infer ctx (TmIf t1 t2 t3) = error "TODO"
infer ctx TmZero = error "TODO"
infer ctx (TmSucc t) = error "TODO"
infer ctx (TmPred t) = error "TODO"
infer ctx (TmIsZero t) = error "TODO"
infer ctx (TmVar k) = error "TODO"
infer ctx (TmApp t1 t2) = error "TODO"
infer ctx (TmAbs t) = error "TODO"
infer ctx (TmAnnAbs ty t) = error "TODO"
infer _ t = inferFail $ "Unsupported term (not required)"

inferType :: Term -> Either String Type
inferType t =
  case runInferM (infer [] t) of
    InferOk (ty, cs) _ ->
      case unifyConstraints cs of
        UnifyOk subst -> Right $ substType subst ty
        UnifyError err -> Left err
    InferError err -> Left err
