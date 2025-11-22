{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Imperative where

{-
Part 1 Tasks:

Imperative Functional Programming
  (a) Define `lit` and `var`
  (b) Define `def`
  (c) Define `+=`, `-=`, and `*=`
  (d) Define `while`
  (e) Ensure the correctness of imperative style code

Replace all error "TODO" with a valid definition.

IMPORTANT: You are NOT allowed to modify existing imports or add new imports.
-}

import Data.Maybe (Maybe (..), fromJust, fromMaybe, isJust)
import Control.Monad (ap)
import Prelude

  ( Applicative (..),
    Bool (..),
    Eq (..),
    Functor (..),
    Int,
    Monad (..),
    Ord (..),
    Show (..),
    String,
    fst,
    snd,
    last,
    lookup,
    length,
    undefined,
    ($),
    (*),
    (+),
    (++),
    (-),
    (<$>),
    (==),
    (.),
    error,
  )

{--------------------------------------------------------
  The state monad definition and operations
  You do not need to modify this code.
--------------------------------------------------------}

data State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State f >>= g = State (\s -> let (a, s') = f s in runState (g a) s')

instance Functor (State s) where
  fmap f (State g) = State (\s -> let (a, s') = g s in (f a, s'))

instance Applicative (State s) where
  pure = return
  (<*>) = ap

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))


{--------------------------------------------------------
  Value and context
--------------------------------------------------------}

data Value = Var Int | Lit Int
  deriving (Show, Eq)

{-
  `lookupCtx` takes an index and a context, and returns the value of the variable at that index.
-}

type Ctx = [(Int, Int)]

lookupCtx :: Int -> Ctx -> Int
lookupCtx i ctx = fromJust (lookup i ctx)


{--------------------------------------------------------
(a) Define `lit` and `var`
--------------------------------------------------------}

lit :: Int -> Value
lit = error "TODO"

var :: Int -> State Ctx Value
var = error "TODO"

{--------------------------------------------------------
(b) Define `def`
--------------------------------------------------------}

def :: State Ctx Value -> Int
def = error "TODO"

{--------------------------------------------------------
(c) Define `+=`, `-=`, and `*=`
--------------------------------------------------------}

(+=) :: Value -> Value -> State Ctx ()
(+=) = error "TODO"

(-=) :: Value -> Value -> State Ctx ()
(-=) = error "TODO"

(*=) :: Value -> Value -> State Ctx ()
(*=) = error "TODO"

{--------------------------------------------------------
(d) Define `while`
--------------------------------------------------------}

while :: Value -> (Int -> Bool) -> State Ctx () -> State Ctx ()
while = error "TODO"

{--------------------------------------------------------
(e) Correctness of Imperative Style Code
    Make sure `factorial` behaves correctly with your implementation.
--------------------------------------------------------}

factorial :: Int -> Int
factorial n = def $ do
  result <- var 1
  i <- var n
  while i (> 0) $ do
    result *= i
    i -= lit 1
  return result
