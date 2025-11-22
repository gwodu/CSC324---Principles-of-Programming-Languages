{-# LANGUAGE InstanceSigs #-}
module InferMonad
  ( InferM(InferM)
  , InferResult(InferOk, InferError)
  , runInferM
  , freshTypeVar
  , inferFail
  )
where

import Type
import Control.Monad (ap)

{-------------------------------------------------------------
  Monad for type inference and its utility functions
----------------------------------------------------------------}

type FreshId = Int

data InferM a = InferM (FreshId -> InferResult a)

data InferResult a
  = InferOk a FreshId
  | InferError String
  deriving (Show, Eq)

instance Functor InferM where
  fmap :: (a -> b) -> InferM a -> InferM b
  fmap = error "TODO"

instance Monad InferM where
  return :: a -> InferM a
  return = error "TODO"

  (>>=) :: InferM a -> (a -> InferM b) -> InferM b
  (>>=) = error "TODO"

runInferM :: InferM a -> InferResult a
runInferM (InferM m) = m initialFreshId
  where
    initialFreshId = 0

freshId :: InferM FreshId
freshId = error "TODO"

freshTypeVar :: InferM TypeVar
freshTypeVar = TypeVar <$> freshId

inferFail :: String -> InferM a
inferFail msg = InferM $ \_ -> InferError msg

{-------------------------------------------------------------
  Applicative instance for InferM
  You do not need to modify this code.
----------------------------------------------------------------}

instance Applicative InferM where
  pure = return
  (<*>) = ap
