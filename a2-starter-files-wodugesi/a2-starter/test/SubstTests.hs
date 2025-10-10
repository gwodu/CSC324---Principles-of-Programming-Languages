module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Term
import DeBruijn

test_lc_unit_public :: TestTree
test_lc_unit_public =
  testGroup "subst LC public unit tests"
  [ testCase "Var 1" $ subst 0 TmZero (TmVar 0) @?= TmZero
  , testCase "Var 2" $ subst 1 TmZero (TmVar 0) @?= TmVar 0
  , testCase "Abs 1" $ subst 0 TmZero (TmAbs (TmVar 1)) @?= TmAbs TmZero
  , testCase "Abs 2" $ subst 0 TmZero (TmAbs (TmVar 0)) @?= (TmAbs (TmVar 0))
  , testCase "App 1" $ subst 0 (TmVar 10) (TmApp (TmVar 0) (TmVar 0)) @?= (TmApp (TmVar 10) (TmVar 10))
  , testCase "App 2" $ subst 0 (TmVar 10) (TmApp (TmVar 0) (TmVar 1)) @?= (TmApp (TmVar 10) (TmVar 1))
  ]

test_mixed_unit_public :: TestTree
test_mixed_unit_public =
  testGroup "subst LC + Arith public unit tests"
  [ testCase "If" $ subst 0 TmTrue (TmIf (TmVar 0) (TmVar 0) (TmVar 0)) @?=
    TmIf TmTrue TmTrue TmTrue
  , testCase "Succ" $ subst 0 (TmVar 42) (TmSucc (TmAbs (TmVar 1))) @?=
    (TmSucc (TmAbs (TmVar 43)))
  , testCase "Pred" $ subst 0 (TmSucc TmZero) (TmPred (TmAbs (TmVar 0))) @?=
    (TmPred (TmAbs (TmVar 0)))
  , testCase "IsZero" $ subst 0 TmZero (TmIsZero (TmApp (TmAbs (TmVar 0)) (TmVar 0))) @?=
    (TmIsZero (TmApp (TmAbs (TmVar 0)) TmZero))
  ]

main :: IO ()
main = defaultMain $
   testGroup "All tests"
   [ test_lc_unit_public
   , test_mixed_unit_public
   ]
