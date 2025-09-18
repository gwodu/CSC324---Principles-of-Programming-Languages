module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Term
import BigStep

bigStepOk :: String -> Term -> Value -> TestTree
bigStepOk name input expected =
  testCase name $ bigStep input @?= EvalOk expected

isEvalError :: EvalResult -> Bool
isEvalError (EvalError _) = True
isEvalError _ = False

bigStepError :: String -> Term -> TestTree
bigStepError name input =
  testCase name $ assertBool "Did not fail as expected" $ isEvalError (bigStep input)

test_value_public :: TestTree
test_value_public =
  testGroup "Public value tests"
  [ bigStepOk "Bool" TmTrue VTrue
  , bigStepOk "Numerical" TmZero (VNum VZero)
  ]

test_if_public :: TestTree
test_if_public =
  testGroup "If public unit tests"
  [ bigStepOk "Simple" (TmIf TmTrue TmZero (TmSucc TmZero)) (VNum VZero)
  , bigStepOk "Simple" (TmIf TmFalse TmZero (TmSucc TmZero)) (VNum (VSucc VZero))
  , bigStepOk "Nested"
    (TmIf (TmIf TmTrue TmTrue TmFalse) TmZero (TmSucc TmZero))
    (VNum VZero)
  ]

test_arith_public :: TestTree
test_arith_public =
  testGroup "Arithmetic public unit tests"
  [ bigStepOk "Simple 1" (TmPred TmZero) (VNum VZero)
  , bigStepOk "Simple 2" (TmPred (TmSucc (TmSucc TmZero))) (VNum (VSucc VZero))
  , bigStepOk "Simple 3" (TmIsZero TmZero) VTrue
  , bigStepOk "Simple 4" (TmIsZero (TmSucc TmZero)) VFalse
  ]

test_complex_public :: TestTree
test_complex_public =
  testGroup "Complex public unit tests"
  [ bigStepOk "If IsZero 1"
    (TmIf (TmIsZero (TmSucc TmZero)) TmZero (TmSucc TmZero))
    (VNum (VSucc VZero))
  , bigStepOk "If IsZero 2"
    (TmIf TmTrue (TmIsZero TmZero) TmFalse)
    VTrue

  , bigStepOk "IsZero Pred 1" (TmIsZero (TmPred TmZero)) VTrue
  , bigStepOk "IsZero Pred 2" (TmIsZero (TmPred (TmSucc TmZero))) VTrue

  , bigStepOk "IsZero If 1"
    (TmIsZero (TmIf TmTrue TmZero (TmSucc TmZero))) VTrue
  , bigStepOk "IsZero If 2"
    (TmIsZero (TmIf TmFalse TmZero (TmSucc TmZero))) VFalse

  , bigStepOk "Pred If 1"
    (TmPred (TmIf TmTrue TmZero (TmSucc (TmSucc TmZero))))
    (VNum VZero)
  ]

test_error :: TestTree
test_error =
  testGroup "Expected error unit tests"
  [ bigStepError "Succ non-num" (TmSucc TmTrue)
  ] -- There are other ways a step can fail, consider them carefully


main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ test_value_public
  , test_if_public
  , test_arith_public
  , test_complex_public
  , test_error
  ]
