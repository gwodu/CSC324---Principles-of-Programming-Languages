module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Term
import SmallStep hiding (isVal, isBoolVal, isNumVal, multiStep)

isVal :: Term -> Bool
isVal t = isBoolVal t
          || isNumVal t

isBoolVal :: Term -> Bool
isBoolVal TmTrue = True
isBoolVal TmFalse = True
isBoolVal _ = False

isNumVal :: Term -> Bool
isNumVal TmZero = True
isNumVal (TmSucc t) = isNumVal t
isNumVal _ = False

multiStep :: Term -> Term
multiStep t = if isVal t
              then t
              else case oneStep t of
                     StepOk t' -> multiStep t'
                     StepError _ -> t

multiStepEq :: String -> Term -> Term -> TestTree
multiStepEq name input expected =
  testCase name $ multiStep input @?= expected

test_value_public :: TestTree
test_value_public =
  testGroup "Public value tests"
  [ multiStepEq "Multi step Bool" TmTrue TmTrue
  , multiStepEq "Multi step Numerical" TmZero TmZero
  ]

test_if_public :: TestTree
test_if_public =
  testGroup "If public unit tests"
  [ multiStepEq "Multi step Simple" (TmIf TmTrue TmZero (TmSucc TmZero)) TmZero
  , multiStepEq "Multi step Simple" (TmIf TmFalse TmZero (TmSucc TmZero)) (TmSucc TmZero)
  , multiStepEq "Multi step Nested"
    (TmIf (TmIf TmTrue TmTrue TmFalse) TmZero (TmSucc TmZero))
    TmZero
  ]

test_arith_public :: TestTree
test_arith_public =
  testGroup "Arithmetic public unit tests"
  [ multiStepEq "Multi step Simple 1" (TmPred TmZero) TmZero
  , multiStepEq "Multi step Simple 2" (TmPred (TmSucc (TmSucc TmZero))) (TmSucc TmZero)
  , multiStepEq "Multi step Simple 3" (TmIsZero TmZero) TmTrue
  , multiStepEq "Multi step Simple 4" (TmIsZero (TmSucc TmZero)) TmFalse
  ]

test_complex_public :: TestTree
test_complex_public =
  testGroup "Complex public unit tests"
  [ multiStepEq "Multi Step If IsZero 1"
    (TmIf (TmIsZero (TmSucc TmZero)) TmZero (TmSucc TmZero))
    (TmSucc TmZero)
  , multiStepEq "Multi Step If IsZero 2"
    (TmIf TmTrue (TmIsZero TmZero) TmFalse)
    TmTrue

  , multiStepEq "Multi Step IsZero Pred 1" (TmIsZero (TmPred TmZero)) TmTrue
  , multiStepEq "Multi Step IsZero Pred 2" (TmIsZero (TmPred (TmSucc TmZero))) TmTrue

  , multiStepEq "Multi Step IsZero If 1"
    (TmIsZero (TmIf TmTrue TmZero (TmSucc TmZero))) TmTrue
  , multiStepEq "Multi Step IsZero If 2"
    (TmIsZero (TmIf TmFalse TmZero (TmSucc TmZero))) TmFalse

  , multiStepEq "Multi Step Pred If 1"
    (TmPred (TmIf TmTrue TmZero (TmSucc (TmSucc TmZero))))
    TmZero

  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ test_value_public
  , test_if_public
  , test_arith_public
  , test_complex_public
  ]
