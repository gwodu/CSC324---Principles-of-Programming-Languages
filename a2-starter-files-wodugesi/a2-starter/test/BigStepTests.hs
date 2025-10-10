module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Term
import BigStep

bigStepOk :: String -> Env -> NamedTerm -> Value -> TestTree
bigStepOk name env input expected =
  testCase name $ bigStep env input @?= EvalOk expected

isEvalError :: EvalResult -> Bool
isEvalError (EvalError _) = True
isEvalError _ = False

bigStepError :: String -> Env -> NamedTerm -> TestTree
bigStepError name env input =
  testCase name $ assertBool "Did not fail as expected" $ isEvalError (bigStep env input)

test_closure_implementation :: TestTree
test_closure_implementation =
  testGroup "Guide on implementing closure"
  [ bigStepOk "Guide 1" []
    (NTAbs "x" (NTVar "x"))
    (Closure [] "x" (NTVar "x"))
  , bigStepOk "Guide 2" []
    (NTAbs "x" (NTAbs "y" (NTVar "x")))
    (Closure [] "x" (NTAbs "y" (NTVar "x")))
  , bigStepOk "Guide 3" []
    (NTApp (NTAbs "x" (NTAbs "y" (NTVar "x"))) NTTrue)
    (Closure [("x", VTrue)] "y" (NTVar "x"))
  , bigStepOk "Guide 4" []
    (NTApp (NTApp (NTApp (NTAbs "w" (NTAbs "x" (NTAbs "y" (NTAbs "z" (NTVar "w"))))) NTZero) NTTrue) NTFalse)
    (Closure [("y", VFalse), ("x", VTrue), ("w", VNum VZero)] "z" (NTVar "w"))
  ]

test_bigStep_unit_public :: TestTree
test_bigStep_unit_public =
  testGroup "bigStep public unit tests"
  [ bigStepOk "Simple 1" []
    (NTApp (NTIf NTTrue (NTAbs "x" (NTVar "x")) (NTAbs "y" (NTVar "y"))) NTTrue)
    VTrue
  , bigStepOk "Simple 2" []
    (NTApp (NTAbs "x" (NTVar "x")) (NTPred NTZero))
    (VNum VZero)
  , bigStepOk "Simple 3" []
    (NTApp (NTAbs "x" (NTVar "x")) NTZero)
    (VNum VZero)
  , bigStepOk "Simple 4" []
    (NTApp (NTAbs "x" (NTVar "x")) (NTAbs "y" (NTVar "z")))
    (Closure [] "y" (NTVar "z"))
  , bigStepOk "Backward 1" []
    (NTIf (NTApp (NTIf NTTrue (NTAbs "x" (NTVar "x")) (NTAbs "y" (NTVar "y"))) NTTrue) NTTrue NTFalse)
    VTrue
  , bigStepOk "Backward 2" []
    (NTIf (NTApp (NTAbs "x" (NTVar "x")) (NTIsZero NTZero)) NTTrue NTFalse)
    VTrue
  , bigStepOk "Backward 3" []
    (NTIf (NTApp (NTAbs "x" (NTVar "x")) NTTrue) NTTrue NTFalse)
    VTrue
  , bigStepOk "Function application 1" []
    (NTApp (NTAbs "x" (NTSucc (NTSucc (NTVar "x")))) NTZero)
    (VNum (VSucc (VSucc VZero)))
  , bigStepOk "Function application 2" []
    (NTApp (NTAbs "a" (NTIf (NTVar "a") NTTrue NTFalse)) NTTrue)
    VTrue
  , bigStepOk "Non-empty Env 1" [("x", VTrue)]
    (NTVar "x")
    VTrue
  , bigStepOk "Non-empty Env 2" [("x", VTrue), ("y", VNum VZero), ("z", VNum (VSucc VZero))]
    (NTIf (NTVar "x") (NTVar "y") (NTVar "z"))
    (VNum VZero)
  , bigStepOk "Non-empty Env 3" [("x", VTrue)]
    (NTAbs "y" (NTVar "x"))
    (Closure [("x", VTrue)] "y" (NTVar "x"))
  ]

test_bigStep_hard_public :: TestTree
test_bigStep_hard_public =
  testGroup "Tricky big step public unit tests"
  [ bigStepOk "Env" [("x", VTrue)]
    (NTApp (NTAbs "y" (NTVar "x")) (NTAbs "z" (NTVar "x")))
    VTrue
  , bigStepOk "Shadow" []
    (NTApp (NTApp (NTAbs "x" (NTAbs "x" (NTVar "x"))) NTTrue) NTFalse)
    VFalse
  , bigStepOk "Capture" [("y", VTrue)]
    (NTApp (NTAbs "y" (NTApp (NTVar "y" ) NTFalse)) (NTAbs "x" (NTVar "y")))
    VTrue
  , bigStepError "Open term" []
    (NTVar "x")
  ]

main :: IO ()
main = defaultMain $
  testGroup "All tests"
  [ test_closure_implementation
  , test_bigStep_unit_public
  , test_bigStep_hard_public
  ]
