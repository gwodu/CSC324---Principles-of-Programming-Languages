module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Term
import Type
import TypeCheck

-- Test for successful type checking
tcOkTest :: String -> TcContext -> Term -> Type -> TestTree
tcOkTest name ctx term expectedType =
  testCase name $ typeCheck ctx term @?= (TcOk expectedType)

-- Test for type checking errors (ignoring the error message)
isTcError :: TcResult -> Bool
isTcError (TcError _) = True
isTcError _ = False

tcErrorTest :: String -> TcContext -> Term -> TestTree
tcErrorTest name ctx term =
  testCase name $ assertBool "Did not fail as expected" $ isTcError (typeCheck ctx term)

-- Basic type tests
test_basic_public :: TestTree
test_basic_public  =
  testGroup "Basic public unit tests"
  [ tcOkTest "TmTrue has type Bool" [] TmTrue TyBool
  , tcOkTest "TmFalse has type Bool" [] TmFalse TyBool
  , tcOkTest "TmZero has type Nat" [] TmZero TyNat
  ]

-- Arithmetic operation tests
test_arith_public :: TestTree
test_arith_public =
  testGroup "Arith public unit tests"
  [ tcOkTest "Succ Zero has type Nat" [] (TmSucc TmZero) TyNat
  , tcOkTest "Pred Zero has type Nat" [] (TmPred TmZero) TyNat
  , tcOkTest "IsZero Zero has type Bool" [] (TmIsZero TmZero) TyBool
  , tcErrorTest "Succ True should fail" [] (TmSucc TmTrue)
  , tcErrorTest "Pred False should fail" [] (TmPred TmFalse)
  , tcErrorTest "IsZero True should fail" [] (TmIsZero TmTrue)
  ]

-- Conditional tests
test_cond_public :: TestTree
test_cond_public =
  testGroup "Conditional public unit tests"
  [ tcOkTest "If True then True else False"
    []
    (TmIf TmTrue TmTrue TmFalse) TyBool
  , tcErrorTest "If-expression with different branch types should fail"
    []
    (TmIf TmTrue TmTrue TmZero)
  , tcErrorTest "If-expression with non-boolean condition should fail"
    []
    (TmIf TmZero TmTrue TmFalse)
  ]

-- Variable and context tests
test_var_public :: TestTree
test_var_public =
  testGroup "Variable public unit tests"
  [ tcOkTest "Variable 0 in context [Bool]" [TyBool] (TmVar 0) TyBool
  , tcOkTest "Variable 1 in context [Nat, Bool]" [TyNat, TyBool] (TmVar 1) TyBool
  , tcErrorTest "Variable 0 in empty context should fail" [] (TmVar 0)
  ]

-- Lambda abstraction tests
test_lambda_public :: TestTree
test_lambda_public =
  testGroup "Lambda public unit tests"
  [ tcOkTest "Identity function on Bool"
    []
    (TmAnnAbs TyBool (TmVar 0)) (TyArrow TyBool TyBool)
  , tcOkTest "Constant function"
    []
    (TmAnnAbs TyBool TmZero)
    (TyArrow TyBool TyNat)
  , tcOkTest "Function composition"
    []
    (TmAnnAbs TyBool (TmAnnAbs TyNat (TmVar 1)))
    (TyArrow TyBool (TyArrow TyNat TyBool))
  ]

-- Function application tests
test_app_public :: TestTree
test_app_public =
  testGroup "Application public unit tests"
  [ tcOkTest "Apply identity to True"
    []
    (TmApp (TmAnnAbs TyBool (TmVar 0)) TmTrue)
    TyBool
  , tcOkTest "Apply constant function"
    []
    (TmApp (TmAnnAbs TyBool TmZero) TmTrue)
    TyNat
  , tcErrorTest "Application of non-function should fail"
    []
    (TmApp TmTrue TmFalse)
  , tcErrorTest "Application of function to wrong type should fail"
    []
    (TmApp (TmAnnAbs TyBool (TmVar 0)) TmZero)
  ]

-- Record tests
test_record_public :: TestTree
test_record_public =
  testGroup "Record public unit tests"
  [ tcOkTest "Empty record"
    []
    (TmRecord [])
    (TyRecord [])
  , tcOkTest "Simple record"
    []
    (TmRecord [("x", TmTrue), ("y", TmZero)])
    (TyRecord [("x", TyBool), ("y", TyNat)])
  , tcOkTest "Record projection"
    []
    (TmProj (TmRecord [("x", TmTrue), ("y", TmZero)]) "x")
    TyBool
  , tcErrorTest "Projection of non-existent field should fail"
    []
    (TmProj (TmRecord [("x", TmTrue)]) "y")
  , tcErrorTest "Projection from non-record should fail"
    []
    (TmProj TmTrue "x")
  ]


-- Pair tests
test_pair_public :: TestTree
test_pair_public =
  testGroup "Pair public unit tests"
  [ tcOkTest "Empty pair" [] (TmPair TmTrue TmZero) (TyProd TyBool TyNat)
  , tcOkTest "Simple pair" [] (TmPair TmTrue TmZero) (TyProd TyBool TyNat)
  , tcOkTest "Project first field" [] (TmFst (TmPair TmTrue TmZero)) TyBool
  , tcOkTest "Project second field" [] (TmSnd (TmPair TmTrue TmZero)) TyNat
  , tcErrorTest "Projection from non-pair should fail" [] (TmFst TmTrue)
  ]

-- Sum tests
test_sum_public :: TestTree
test_sum_public =
  testGroup "Sum public unit tests"
  [ tcOkTest "Simple sum" [] (TmInl TmTrue (TySum TyBool TyNat)) (TySum TyBool TyNat)
  , tcOkTest "Simple sum" [] (TmInr TmZero (TySum TyBool TyNat)) (TySum TyBool TyNat)
  , tcOkTest "Match on sum" [] (TmMatch (TmInl TmTrue (TySum TyBool TyNat)) (TmVar 0) TmTrue) TyBool
  , tcErrorTest "Match on sum should have same type" [] (TmMatch (TmInr TmZero (TySum TyBool TyNat)) (TmVar 0) (TmVar 0))
  ]


main :: IO ()
main = defaultMain $
  testGroup "All tests"
  [ test_basic_public
  , test_arith_public
  , test_cond_public
  , test_var_public
  , test_lambda_public
  , test_app_public
  , test_record_public
  , test_pair_public
  , test_sum_public
  ]
