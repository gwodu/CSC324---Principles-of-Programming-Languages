module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Map (fromList)

-- Imports from the project
import Type
import TypeCheck
import Term
import Infer
import Data.List (nub, sort)

{-------------------------------------------------------------
  Helper functions for testing
----------------------------------------------------------------}

tcOkTest :: String -> TcContext -> Term -> Type -> TestTree
tcOkTest name ctx term expectedType = testCase name $
  let result = typeCheck ctx term in
  result @?= TcOk expectedType

tcErrorTest :: String -> TcContext -> Term -> TestTree
tcErrorTest name ctx term = testCase name $
  let result = typeCheck ctx term in
  case result of
    TcError _ -> return ()
    TcOk ty -> assertFailure $ "Expected error, but got type " ++ show ty

subtypeTest :: String -> Type -> Type -> Bool -> TestTree
subtypeTest name t1 t2 expected = testCase name $
  (t1 <: t2) @?= expected

inferenceTest :: String -> Term -> Type -> TestTree
inferenceTest name term expectedType = testCase name $
  case inferType term of
    Right inferredType -> assertBool
      ("Type mismatch: expected " ++ show expectedType ++ " but got " ++ show inferredType)
      (equalUpToRenaming expectedType inferredType)
    Left err -> assertFailure $ "Inference failed unexpectedly: " ++ err

inferenceErrorTest :: String -> Term -> TestTree
inferenceErrorTest name term = testCase name $
  case inferType term of
    Right _ -> assertFailure $ name ++ ": Expected inference to fail"
    Left _ -> return ()


type Rename = [(Int, Int)]

equalUpToRenaming :: Type -> Type -> Bool
equalUpToRenaming ty1 ty2 = maybe False isIsomorphism (generateRenaming ty1 ty2)
  where
    generateRenaming :: Type -> Type -> Maybe Rename
    generateRenaming t1 t2 = case (t1, t2) of
      (TyBool, TyBool) -> Just []
      (TyNat, TyNat) -> Just []
      (TyVar (TypeVar x), TyVar (TypeVar y)) -> Just [(x, y)]
      (TyArrow t11 t12, TyArrow t21 t22) -> do
        f1 <- generateRenaming t11 t21
        f2 <- generateRenaming t12 t22
        return (f1 ++ f2)
      _ -> Nothing
    
    -- Check if a rename is an isomorphism
    isIsomorphism :: Rename -> Bool
    isIsomorphism f = let g = nub f
      in let len = length g
      in let l = nub (map fst g)
      in let r = nub (map snd g)
      in len == length l && len == length r


{-------------------------------------------------------------
  Tests for Type.hs functions
----------------------------------------------------------------}

-- Tests for substType
substTypeTests :: TestTree
substTypeTests = testGroup "substType Tests"
  [ testCase "substType identity" $
      substType emptySubst TyBool @?= TyBool,
    testCase "substType simple variable" $
      substType (Subst (fromList [(TypeVar 0, TyBool)])) (TyVar (TypeVar 0)) @?= TyBool,
    testCase "substType unbound variable" $
      substType (Subst (fromList [(TypeVar 0, TyBool)])) (TyVar (TypeVar 1)) @?= TyVar (TypeVar 1),
    testCase "substType arrow type" $
      substType (Subst (fromList [(TypeVar 0, TyBool), (TypeVar 1, TyNat)]))
                (TyArrow (TyVar (TypeVar 0)) (TyVar (TypeVar 1))) @?= TyArrow TyBool TyNat,
    testCase "substType nested substitution" $
      substType (Subst (fromList [(TypeVar 0, TyBool)]))
                (TyArrow (TyVar (TypeVar 0)) (TyVar (TypeVar 0))) @?= TyArrow TyBool TyBool
  ]

-- Tests for composeSubst
composeSubstTests :: TestTree
composeSubstTests = testGroup "composeSubst Tests"
  [ testCase "composeSubst with empty" $
      composeSubst (Subst (fromList [(TypeVar 0, TyBool)])) emptySubst @?= Subst (fromList [(TypeVar 0, TyBool)]),
    testCase "composeSubst transitivity" $
      substType (composeSubst (Subst (fromList [(TypeVar 1, TyNat)]))
                              (Subst (fromList [(TypeVar 0, TyVar (TypeVar 1))])))
                (TyVar (TypeVar 0)) @?=
      substType (Subst (fromList [(TypeVar 0, TyNat), (TypeVar 1, TyNat)])) (TyVar (TypeVar 0)),
    testCase "composeSubst independent variables" $
      let s1 = Subst (fromList [(TypeVar 0, TyBool)])
          s2 = Subst (fromList [(TypeVar 1, TyNat)])
          composed = composeSubst s1 s2
          testType = TyArrow (TyVar (TypeVar 0)) (TyVar (TypeVar 1))
          expected = TyArrow TyBool TyNat
      in substType composed testType @?= expected
  ]

-- Tests for ftv
ftvTests :: TestTree
ftvTests = testGroup "ftv Tests"
  [ testCase "ftv Bool" $
      ftv TyBool @?= [],
    testCase "ftv Nat" $
      ftv TyNat @?= [],
    testCase "ftv variable" $
      ftv (TyVar (TypeVar 0)) @?= [TypeVar 0],
    testCase "ftv arrow" $
      sort (ftv (TyArrow (TyVar (TypeVar 0)) (TyVar (TypeVar 1)))) @?= sort [TypeVar 0, TypeVar 1],
    testCase "ftv duplicate variables" $
      sort (nub (ftv (TyArrow (TyVar (TypeVar 0)) (TyVar (TypeVar 0))))) @?= sort [TypeVar 0]
  ]

{-------------------------------------------------------------
  Tests for TypeCheck.hs subtyping
----------------------------------------------------------------}

-- Tests for subtyping relation (<:)
subtypingTests :: TestTree
subtypingTests = testGroup "Subtyping Tests"
  [ -- Reflexivity
    subtypeTest "Bool <: Bool" TyBool TyBool True,
    subtypeTest "Nat <: Nat" TyNat TyNat True,

    -- Top type
    subtypeTest "Bool <: Top" TyBool TyTop True,
    subtypeTest "Nat <: Top" TyNat TyTop True,
    subtypeTest "Top <: Bool should fail" TyTop TyBool False,

    -- Arrow subtyping (contravariant in argument, covariant in result)
    subtypeTest "Arrow subtyping covariant"
      (TyArrow TyBool TyBool) (TyArrow TyBool TyTop) True,
    subtypeTest "Arrow subtyping contravariant"
      (TyArrow TyTop TyBool) (TyArrow TyBool TyBool) True,
    subtypeTest "Arrow subtyping both"
      (TyArrow TyTop TyBool) (TyArrow TyBool TyTop) True,
    subtypeTest "Arrow subtyping violation"
      (TyArrow TyBool TyTop) (TyArrow TyTop TyBool) False,

    -- Record subtyping (width and depth)
    subtypeTest "Record width subtyping"
      (TyRecord [("x", TyBool), ("y", TyNat)]) (TyRecord [("x", TyBool)]) True,
    subtypeTest "Record depth subtyping"
      (TyRecord [("x", TyBool)]) (TyRecord [("x", TyTop)]) True,
    subtypeTest "Record missing field"
      (TyRecord [("y", TyBool)]) (TyRecord [("x", TyBool)]) False,

    -- Product subtyping
    subtypeTest "Product subtyping"
      (TyProd TyBool TyBool) (TyProd TyTop TyBool) True,
    subtypeTest "Product subtyping failure"
      (TyProd TyTop TyBool) (TyProd TyBool TyTop) False,

    -- Sum subtyping
    subtypeTest "Sum subtyping"
      (TySum TyBool TyBool) (TySum TyTop TyBool) True,
    subtypeTest "Sum subtyping failure"
      (TySum TyTop TyBool) (TySum TyBool TyTop) False
  ]


typeCheckSubtypingTests :: TestTree
typeCheckSubtypingTests = testGroup "TypeCheck Subtyping Tests"
  [ -- Function application with subtyping
    tcOkTest "Function app with subtype argument" []
             (TmApp (TmAnnAbs TyTop TmZero) TmTrue) TyNat,

    -- Record subtyping
    tcOkTest "Record projection with extra fields" []
             (TmApp (TmAnnAbs (TyRecord [("x", TyBool)]) (TmProj (TmVar 0) "x")) (TmRecord [("x", TmTrue), ("y", TmZero)])) TyBool,

    -- Sum injection with subtyping using concrete values
    tcOkTest "Inject True to Top+Nat" []
             (TmInl TmTrue (TySum TyTop TyNat))
             (TySum TyTop TyNat),

    -- Error cases
    tcErrorTest "Function app subtyping violation" []
                (TmApp (TmAnnAbs TyBool TmZero) TmZero),
    
    tcErrorTest "If requires both branches to be the same type"
                []
                (TmIf TmTrue TmZero TmTrue)
  ]

{-------------------------------------------------------------
  Tests for type inference
----------------------------------------------------------------}

inferenceTests :: TestTree
inferenceTests = testGroup "Type Inference Tests"
  [ -- Basic terms
    inferenceTest "True is Bool" TmTrue TyBool,
    inferenceTest "Zero is Nat" TmZero TyNat,

    -- Simple abstractions
    inferenceTest "Identity function" (TmAbs (TmVar 0)) (TyArrow (TyVar (TypeVar 0)) (TyVar (TypeVar 0))),

    -- Function application with concrete types
    inferenceTest "Application inference"
                  (TmApp (TmAbs (TmVar 0)) TmTrue)
                  TyBool,

    inferenceTest "Higher-order input"
                  (TmAbs (TmApp (TmVar 0) TmZero))
                  (TyArrow (TyArrow TyNat (TyVar (TypeVar 0))) (TyVar (TypeVar 0))),

    inferenceTest "Const function" (TmAbs (TmAbs (TmVar 1))) (TyArrow (TyVar (TypeVar 0)) (TyArrow (TyVar (TypeVar 1)) (TyVar (TypeVar 0)))),

    inferenceTest "Function with Nat"
                  (TmAbs (TmSucc (TmVar 0)))
                  (TyArrow TyNat TyNat),

    -- Conditional inference
    inferenceTest "If-then-else inference"
                  (TmIf TmTrue TmZero (TmSucc TmZero))
                  TyNat,

    -- Annotated abstractions
    inferenceTest "Annotated identity"
                          (TmAnnAbs TyBool (TmVar 0))
                          (TyArrow TyBool TyBool),

    -- Error cases
    inferenceErrorTest "Self-application" (TmAbs (TmApp (TmVar 0) (TmVar 0))),

    inferenceErrorTest "Type mismatch in if"
                       (TmIf TmTrue TmZero TmFalse),

    inferenceErrorTest "Function application with wrong type"
                       (TmApp (TmAbs (TmApp (TmVar 0) TmZero)) TmTrue)
  ]

{-------------------------------------------------------------
  Test suite main function
----------------------------------------------------------------}

main :: IO ()
main = defaultMain $ testGroup "Part 2 and 3 Tests"
  [ substTypeTests,
    composeSubstTests,
    ftvTests,
    subtypingTests,
    typeCheckSubtypingTests,
    inferenceTests
  ]
