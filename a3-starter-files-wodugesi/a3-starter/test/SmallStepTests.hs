module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Term
import Type
import SmallStep hiding (isVal, isBoolVal, isNumVal, isLambdaForm, multiStep, isRecordVal, isPairVal, isSumVal)

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
isLambdaForm (TmAnnAbs _ _) = True
isLambdaForm _ = False

isRecordVal :: Term -> Bool
isRecordVal (TmRecord pairs) = all (isVal . snd) pairs
isRecordVal _ = False

isPairVal :: Term -> Bool
isPairVal (TmPair t1 t2) = isVal t1 && isVal t2
isPairVal _ = False

isSumVal :: Term -> Bool
isSumVal (TmInl t _) = isVal t
isSumVal (TmInr t _) = isVal t
isSumVal _ = False

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

test_value_public :: TestTree
test_value_public =
  testGroup "Value public unit tests"
  [ oneStepError "Pair" (TmPair TmZero (TmSucc TmZero))
  , oneStepError "Record" (TmRecord [("L1", TmTrue), ("L2", TmZero)])
  , oneStepError "Inl" (TmInl TmZero (TySum TyNat TyNat))
  ]

test_lambda_public :: TestTree
test_lambda_public =
  testGroup "Lambda public unit tests"
  [ oneStepOk "Simple 1"
    (TmApp (TmAnnAbs TyNat (TmVar 0)) (TmZero))
    TmZero
  , oneStepOk "Simple 2"
    (TmApp (TmAnnAbs (TyArrow TyNat TyNat) (TmVar 0)) (TmAnnAbs TyNat (TmVar 0)))
    (TmAnnAbs TyNat (TmVar 0))
  ]

test_record_public :: TestTree
test_record_public =
  testGroup "Record public unit tests"
  [ oneStepOk "Simple 1"
    (TmRecord [("Label 1", TmPred TmZero), ("Label 2", TmPred TmZero)])
    (TmRecord [("Label 1", TmZero), ("Label 2", TmPred TmZero)])
  , oneStepOk "Simple 2"
    (TmRecord [("Label 1", TmZero), ("Label 2", TmPred TmZero)])
    (TmRecord [("Label 1", TmZero), ("Label 2", TmZero)])
  , oneStepOk "Simple 3"
    (TmProj (TmRecord [("Label 1", TmZero), ("Label 2", TmTrue)]) "Label 1")
    TmZero
  ]

test_pair_public :: TestTree
test_pair_public =
  testGroup "Pair public unit tests"
  [ oneStepOk "Simple 1"
    (TmPair (TmPred TmZero) (TmPred TmZero))
    (TmPair TmZero (TmPred TmZero))
  , oneStepOk "Simple 2"
    (TmPair TmZero (TmPred TmZero))
    (TmPair TmZero TmZero)
  , oneStepOk "Simple 3"
    (TmFst (TmPair TmZero TmTrue))
    TmZero
  ]

test_sum_public :: TestTree
test_sum_public =
  testGroup "Sum public unit tests"
  [ oneStepOk "Simple 1"
    (TmInl (TmPred TmZero) (TySum TyNat TyBool))
    (TmInl TmZero (TySum TyNat TyBool))
  , oneStepOk "Simple 2"
    (TmMatch (TmInl TmZero (TySum TyNat TyBool)) (TmVar 0) (TmVar 0))
    TmZero
  ]

main :: IO ()
main = defaultMain $
  testGroup "All tests"
  [ test_value_public
  , test_lambda_public
  , test_record_public
  , test_pair_public
  , test_sum_public
  ]
