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
  fmap = error "TODO"

instance Monad UnifyResult where
  return :: a -> UnifyResult a
  return = error "TODO"

  (>>=) :: UnifyResult a -> (a -> UnifyResult b) -> UnifyResult b
  (>>=) = error "TODO"

unifyFail :: String -> UnifyResult a
unifyFail = UnifyError

{--------------------------------------------------------
  Unification
--------------------------------------------------------}

unifyConstraints :: Constraints -> UnifyResult Subst
unifyConstraints = error "TODO"

{-------------------------------------------------------------
  Applicative instance for UnifyResult
  You do not need to modify this code.
----------------------------------------------------------------}

instance Applicative UnifyResult where
  pure = return
  (<*>) = ap
