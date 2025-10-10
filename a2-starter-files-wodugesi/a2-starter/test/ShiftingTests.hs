module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Term
import DeBruijn


test_lc_shifting_unit_public :: TestTree
test_lc_shifting_unit_public =
  testGroup "shifting LC public unit tests"
  [ testCase "Var 1" $ shifting 1 0 (TmVar 0) @?= TmVar 1
  , testCase "Var 2" $ shifting 1 2 (TmVar 0) @?= TmVar 0
  , testCase "Abs 1" $ shifting 1 1 (TmVar 0) @?= TmVar 0
  , testCase "Abs 2" $ shifting 1 0 (TmAbs (TmVar 0)) @?= TmAbs (TmVar 0)
  , testCase "App 1" $ shifting 1 0 (TmApp (TmVar 0) (TmVar 2)) @?= (TmApp (TmVar 1) (TmVar 3))
  , testCase "App 2" $ shifting 1 1 (TmApp (TmVar 0) (TmVar 2)) @?= (TmApp (TmVar 0) (TmVar 3))
  ]

test_mixed_unit_public :: TestTree
test_mixed_unit_public =
  testGroup "shifting LC + Arith public unit tests"
  [ testCase "If" $ shifting 1 1 (TmIf TmTrue TmTrue (TmAbs (TmVar 1))) @?=
    (TmIf TmTrue TmTrue (TmAbs (TmVar 1)))
  , testCase "Succ" $ shifting (-1) 0 (TmSucc (TmVar 1)) @?=
    (TmSucc (TmVar 0))
  , testCase "Pred" $ shifting 42 0 (TmPred (TmVar 1)) @?=
    (TmPred (TmVar 43))
  , testCase "IsZero" $ shifting 5 5 (TmIsZero (TmVar 4)) @?=
    (TmIsZero (TmVar 4))
  ]

main :: IO ()
main = defaultMain $
  testGroup "All tests"
  [ test_lc_shifting_unit_public
  , test_mixed_unit_public
  ]
