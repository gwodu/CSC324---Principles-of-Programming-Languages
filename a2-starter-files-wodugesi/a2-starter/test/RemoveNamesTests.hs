module Main where

import DeBruijn
import Term
import Test.Tasty
import Test.Tasty.HUnit

test_lc_unit_public :: TestTree
test_lc_unit_public =
  testGroup "removeNames Lambda calculus public unit tests"
  [ testCase "Var 1" $ removeNames ["w", "x", "y", "z"] (NTVar "w") @?= (TmVar 0)
  , testCase "Var 2" $ removeNames ["w", "x", "y", "z"] (NTVar "y") @?= (TmVar 2)
  , testCase "Abs 1" $ removeNames ["y", "x"] (NTVar "x") @?= (TmVar 1)
  , testCase "Abs 2" $ removeNames ["x"] (NTAbs "y" (NTVar "x")) @?= (TmAbs (TmVar 1))
  , testCase "Abs 3" $ removeNames [] (NTAbs "x" (NTAbs "y" (NTVar "x"))) @?= (TmAbs (TmAbs (TmVar 1)))
  , testCase "App 1" $ removeNames ["x", "f"] (NTApp (NTVar "f") (NTVar "x")) @?= (TmApp (TmVar 1) (TmVar 0))
  , testCase "Combinator" $
    removeNames [] (NTAbs "x" (NTAbs "y" (NTAbs "z" (NTApp (NTApp (NTVar "x") (NTVar "z")) (NTApp (NTVar "y") (NTVar "z")))))) @?=
    (TmAbs (TmAbs (TmAbs (TmApp (TmApp (TmVar 2) (TmVar 0)) (TmApp (TmVar 1) (TmVar 0))))))
  , testCase "Shadow" $
    removeNames [] (NTAbs "x" (NTAbs "x" (NTAbs "x" (NTVar "x")))) @?=
    (TmAbs (TmAbs (TmAbs (TmVar 0))))
  ]

test_mixed_unit_public :: TestTree
test_mixed_unit_public =
  testGroup "removeNames LC + Arith public unit tests"
  [ testCase "If" $
    removeNames ["y", "z"] (NTIf (NTApp (NTVar "y") (NTVar "z")) (NTAbs "x" (NTVar "x")) (NTAbs "x" (NTVar "z"))) @?=
    (TmIf (TmApp (TmVar 0) (TmVar 1)) (TmAbs (TmVar 0)) (TmAbs (TmVar 2)))
  , testCase "Succ" $
    removeNames ["y", "z"] (NTSucc (NTAbs "x" (NTApp (NTVar "x") (NTVar "z")))) @?=
    (TmSucc (TmAbs (TmApp (TmVar 0) (TmVar 2))))
  , testCase "Pred" $
    removeNames ["y", "z"] (NTPred (NTAbs "x" (NTApp (NTVar "y") (NTVar "z")))) @?=
    (TmPred (TmAbs (TmApp (TmVar 1) (TmVar 2))))
  , testCase "IsZero" $
    removeNames ["y", "z"] (NTIsZero (NTAbs "x" (NTApp (NTVar "y") (NTVar "x")))) @?=
    (TmIsZero (TmAbs (TmApp (TmVar 1) (TmVar 0))))
  ]
main :: IO ()
main = defaultMain $
  testGroup "All tests"
  [ test_lc_unit_public
  , test_mixed_unit_public
  ]
