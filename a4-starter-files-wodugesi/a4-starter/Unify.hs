{-# LANGUAGE InstanceSigs #-}

module Unify where

import Control.Monad (ap)
import Prelude hiding (lookup, map)
import Data.Map (Map, empty, lookup, map, singleton, union)
import Type

{--------------------------------------------------------
  Monad for unification
--------------------------------------------------------}

data UnifyResult a
  = UnifyOk a
  | UnifyError String
  deriving (Show, Eq)

instance Functor UnifyResult where
  fmap :: (a -> b) -> UnifyResult a -> UnifyResult b
  fmap f (UnifyOk a) = UnifyOk (f a)
  fmap _ (UnifyError err) = UnifyError err

instance Monad UnifyResult where
  return :: a -> UnifyResult a
  return = UnifyOk

  (>>=) :: UnifyResult a -> (a -> UnifyResult b) -> UnifyResult b
  (UnifyOk a) >>= f = f a
  (UnifyError err) >>= _ = UnifyError err

unifyFail :: String -> UnifyResult a
unifyFail = UnifyError

{--------------------------------------------------------
  Unification
--------------------------------------------------------}

unifyConstraints :: Constraints -> UnifyResult Subst
unifyConstraints [] = return emptySubst
unifyConstraints (CEq s t : rest)
  | s == t = unifyConstraints rest
  | otherwise = case (s, t) of
      (TyVar x, _) ->
        if x `elem` ftv t
        then unifyFail $ "Occurs check failed: " ++ show x ++ " in " ++ show t
        else do
          let subst = Subst (singleton x t)
          restSubst <- unifyConstraints (substituteConstraints subst rest)
          return (composeSubst restSubst subst)
      (_, TyVar x) ->
        if x `elem` ftv s
        then unifyFail $ "Occurs check failed: " ++ show x ++ " in " ++ show s
        else do
          let subst = Subst (singleton x s)
          restSubst <- unifyConstraints (substituteConstraints subst rest)
          return (composeSubst restSubst subst)
      (TyArrow s1 s2, TyArrow t1 t2) ->
        unifyConstraints (CEq s1 t1 : CEq s2 t2 : rest)
      (TyProd s1 s2, TyProd t1 t2) ->
        unifyConstraints (CEq s1 t1 : CEq s2 t2 : rest)
      (TySum s1 s2, TySum t1 t2) ->
        unifyConstraints (CEq s1 t1 : CEq s2 t2 : rest)
      (TyRecord fs1, TyRecord fs2) ->
        if length fs1 /= length fs2
        then unifyFail $ "Record types have different number of fields"
        else
          let newConstraints = matchFields fs1 fs2
          in case newConstraints of
               Nothing -> unifyFail "Record types have incompatible fields"
               Just cs -> unifyConstraints (cs ++ rest)
      _ -> unifyFail $ "Cannot unify " ++ show s ++ " with " ++ show t
  where
    substituteConstraints :: Subst -> Constraints -> Constraints
    substituteConstraints subst cs = [CEq (substType subst s') (substType subst t') | CEq s' t' <- cs]

    matchFields :: [(String, Type)] -> [(String, Type)] -> Maybe Constraints
    matchFields [] [] = Just []
    matchFields ((l1, t1):fs1') fs2 =
      case findAndRemove l1 fs2 of
        Nothing -> Nothing
        Just (t2, fs2') -> do
          rest <- matchFields fs1' fs2'
          return (CEq t1 t2 : rest)
    matchFields _ _ = Nothing

    findAndRemove :: String -> [(String, Type)] -> Maybe (Type, [(String, Type)])
    findAndRemove _ [] = Nothing
    findAndRemove l ((l', t):fs)
      | l == l' = Just (t, fs)
      | otherwise = do
          (t', fs') <- findAndRemove l fs
          return (t', (l', t):fs')

{-------------------------------------------------------------
  Applicative instance for UnifyResult
  You do not need to modify this code.
----------------------------------------------------------------}

instance Applicative UnifyResult where
  pure = return
  (<*>) = ap
