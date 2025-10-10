module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Term
import SmallStep hiding (isVal, isBoolVal, isNumVal, isLambdaForm, multiStep)

isVal :: Term -> Bool
isVal t = isBoolVal t
          || isNumVal t
          || isLambdaForm t

isBoolVal :: Term -> Bool
isBoolVal TmTrue = True
isBoolVal TmFalse = True
isBoolVal _ = False

isNumVal :: Term -> Bool
isNumVal TmZero = True
isNumVal (TmSucc t) = isNumVal t
isNumVal _ = False

isLambdaForm :: Term -> Bool
isLambdaForm (TmAbs _) = True
isLambdaForm _ = False

multiStep :: Term -> Term
multiStep t = if isVal t
              then t
              else case oneStep t of
                     StepOk t' -> multiStep t'
                     StepError _ -> t

oneStepOk :: String -> Term -> Term -> TestTree
oneStepOk name input expected =
  testCase name $ oneStep input @?= StepOk expected

isStepError :: StepResult -> Bool
isStepError (StepError _) = True
isStepError _ = False

oneStepError :: String -> Term -> TestTree
oneStepError name input =
  testCase name $ assertBool "Did not fail as expected" $ isStepError (oneStep input)

multiStepEq :: String -> Term -> Term -> TestTree
multiStepEq name input expected =
  testCase name $ multiStep input @?= expected

test_oneStep_value_unit_public :: TestTree
test_oneStep_value_unit_public =
  testGroup "oneStep value public unit test"
  [ oneStepError "Lambda" (TmAbs (TmVar 0))
  ]

test_oneStep_unit_public :: TestTree
test_oneStep_unit_public =
  testGroup "One-step public unit tests"
  [ oneStepOk "Simple 1" (TmApp (TmIf TmTrue (TmAbs (TmVar 0)) (TmAbs (TmVar 0))) TmTrue) (TmApp (TmAbs (TmVar 0)) TmTrue)
  , oneStepOk "Simple 2" (TmApp (TmAbs (TmVar 0)) (TmPred TmZero)) (TmApp (TmAbs (TmVar 0)) TmZero)
  , oneStepOk "Simple 3" (TmApp (TmAbs (TmVar 0)) TmZero) TmZero
  , oneStepOk "Simple 4" (TmApp (TmAbs (TmVar 0)) (TmAbs (TmVar 42))) (TmAbs (TmVar 42))
  , oneStepOk "Simple 5" (TmApp (TmAbs (TmVar 1)) (TmAbs (TmVar 42))) (TmVar 0)
  , oneStepOk "Backward 1"
    (TmIf (TmApp (TmIf TmTrue (TmAbs (TmVar 0)) (TmAbs (TmVar 0))) TmTrue) TmTrue TmFalse)
    (TmIf (TmApp (TmAbs (TmVar 0)) TmTrue) TmTrue TmFalse)
  , oneStepOk "Backward 2"
    (TmIf (TmApp (TmAbs (TmVar 0)) (TmIsZero TmZero)) TmTrue TmFalse)
    (TmIf (TmApp (TmAbs (TmVar 0)) TmTrue) TmTrue TmFalse)
  , oneStepOk "Backward 3"
    (TmIf (TmApp (TmAbs (TmVar 0)) TmTrue) TmTrue TmFalse)
    (TmIf TmTrue TmTrue TmFalse)
  , oneStepOk "Function application 1"
    (TmApp (TmAbs (TmSucc (TmSucc (TmVar 0)))) TmZero)
    (TmSucc (TmSucc TmZero))
  , oneStepOk "Function application 2"
    (TmApp (TmAbs (TmIf (TmVar 0) TmTrue TmFalse)) TmTrue)
    (TmIf TmTrue TmTrue TmFalse)
  ]

test_multiStep_unit_public :: TestTree
test_multiStep_unit_public =
  testGroup "Multi-step public unit tests"
  [ multiStepEq "Simple 1"
    (TmApp (TmIf TmTrue (TmAbs (TmVar 0)) (TmAbs (TmVar 0))) TmTrue)
    TmTrue
  , multiStepEq "Simple 2"
    (TmApp (TmAbs (TmVar 0)) (TmPred TmZero))
    TmZero
  , multiStepEq "Simple 3"
    (TmApp (TmAbs (TmVar 0)) TmZero)
    TmZero
  , multiStepEq "Simple 4"
    (TmApp (TmAbs (TmVar 0)) (TmAbs (TmVar 42)))
    (TmAbs (TmVar 42))
  , multiStepEq "Simple 5"
    (TmApp (TmAbs (TmVar 1)) (TmAbs (TmVar 42)))
    (TmVar 0)
  , multiStepEq "Backward 1"
    (TmIf (TmApp (TmIf TmTrue (TmAbs (TmVar 0)) (TmAbs (TmVar 0))) TmTrue) TmTrue TmFalse)
    TmTrue
  , multiStepEq "Backward 2"
    (TmIf (TmApp (TmAbs (TmVar 0)) (TmIsZero TmZero)) TmTrue TmFalse)
    TmTrue
  , multiStepEq "Backward 3"
    (TmIf (TmApp (TmAbs (TmVar 0)) TmTrue) TmTrue TmFalse)
    TmTrue
  ,  multiStepEq "Function application 1"
    (TmApp (TmAbs (TmSucc (TmSucc (TmVar 0)))) TmZero)
    (TmSucc (TmSucc TmZero))
  , multiStepEq "Function application 2"
    (TmApp (TmAbs (TmIf (TmVar 0) TmTrue TmFalse)) TmTrue)
    TmTrue
  ]

test_error_public :: TestTree
test_error_public =
  testGroup "Expect error public unit tests"
  [ oneStepError "Var cannot be evaluated"
    (TmVar 42)
  , oneStepError "Free var cannot be used as argument"
    (TmApp (TmAbs (TmVar 0)) (TmVar 42))
  ]

main :: IO ()
main = defaultMain $
  testGroup "All tests"
  [ test_oneStep_value_unit_public
  , test_oneStep_unit_public
  , test_multiStep_unit_public
  , test_error_public
  ]
