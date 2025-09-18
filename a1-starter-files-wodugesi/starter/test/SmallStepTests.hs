module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Term
import SmallStep hiding (isVal, isBoolVal, isNumVal, multiStep)

oneStepOk :: String -> Term -> Term -> TestTree
oneStepOk name input expected =
  testCase name $ oneStep input @?= StepOk expected

isStepError :: StepResult -> Bool
isStepError (StepError _) = True
isStepError _ = False

oneStepError :: String -> Term -> TestTree
oneStepError name input =
  testCase name $ assertBool "Did not fail as expected" $ isStepError (oneStep input)

test_value_public :: TestTree
test_value_public =
  testGroup "Public value tests"
  [ oneStepError "One step Bool" TmTrue
  , oneStepError "Numerical" TmZero
  ]

test_if_public :: TestTree
test_if_public =
  testGroup "If public unit tests"
  [ oneStepOk "One step Simple" (TmIf TmTrue TmZero (TmSucc TmZero)) TmZero
  , oneStepOk "One step Simple" (TmIf TmFalse TmZero (TmSucc TmZero)) (TmSucc TmZero)
  , oneStepOk "One step Nested"
    (TmIf (TmIf TmTrue TmTrue TmFalse) TmZero (TmSucc TmZero))
    (TmIf TmTrue TmZero (TmSucc TmZero))
  ]

test_arith_public :: TestTree
test_arith_public =
  testGroup "Arithmetic public unit tests"
  [ oneStepOk "One step Simple 1" (TmPred TmZero) TmZero
  , oneStepOk "One step Simple 2" (TmPred (TmSucc (TmSucc TmZero))) (TmSucc TmZero)
  , oneStepOk "One step Simple 3" (TmIsZero TmZero) TmTrue
  , oneStepOk "One step Simple 4" (TmIsZero (TmSucc TmZero)) TmFalse
  ]

test_complex_public :: TestTree
test_complex_public =
  testGroup "Complex public unit tests"
  [ oneStepOk "One step If IsZero 1"
    (TmIf (TmIsZero (TmSucc TmZero)) TmZero (TmSucc TmZero))
    (TmIf TmFalse TmZero (TmSucc TmZero))
  , oneStepOk "One step If IsZero 2"
    (TmIf TmTrue (TmIsZero TmZero) TmFalse)
    (TmIsZero TmZero)
  , oneStepOk "One step IsZero Pred 1" (TmIsZero (TmPred TmZero)) (TmIsZero TmZero)
  , oneStepOk "One step IsZero Pred 2" (TmIsZero (TmPred (TmSucc TmZero))) (TmIsZero TmZero)
  , oneStepOk "One step IsZero If 1"
    (TmIsZero (TmIf TmTrue TmZero (TmSucc TmZero)))
    (TmIsZero TmZero)
  , oneStepOk "One step IsZero If 2"
    (TmIsZero (TmIf TmFalse TmZero (TmSucc TmZero)))
    (TmIsZero (TmSucc TmZero))
  , oneStepOk "One step Pred If 1"
    (TmPred (TmIf TmTrue TmZero (TmSucc (TmSucc TmZero))))
    (TmPred TmZero)
  ]

-- Category: Error cases
test_error :: TestTree
test_error =
  testGroup "Expected error unit tests"
  [ oneStepError "Succ non-num" (TmSucc TmTrue)
  ] -- there are other ways a step can fail, think about them carefully

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ test_value_public
  , test_if_public
  , test_arith_public
  , test_complex_public
  , test_error
  ]
