{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Imperative
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain (testGroup "Part 1 Tests" [litVarTests, defTests, opTests, impTests, whileTests])

evalState :: State s a -> s -> a
evalState (State f) s = fst (f s)

emptyCtx :: Ctx
emptyCtx = []

-- (a) `lit` and `var`
litVarTests :: TestTree
litVarTests =
  testGroup
    "lit & var Tests"
    [ testCase "lit shape" $ lit 10 @?= Lit 10,
      testCase "var shape" $ case evalState (var 20 :: State Ctx Value) emptyCtx of
        Lit _ -> assertFailure "var must return a Var"
        Var _ -> return ()
    ]

-- (b) `def`
defTests :: TestTree
defTests =
  testGroup
    "def Tests"
    [ testCase "def lit" $ def (pure (lit 10)) @?= 10,
      testCase "def var" $ def (var 10) @?= 10
    ]

-- (c) `+=`, `-=`, and `*=`

opTestCases :: [Char] -> (Value -> Value -> State Ctx ()) -> Int -> Int -> Int -> [TestTree]
opTestCases name op lhs rhs res =
  [ testCase (name ++ " lit") $ def (do x <- var lhs; x `op` lit rhs; return x) @?= res,
    testCase (name ++ " var") $ def (do x <- var lhs; y <- var rhs; x `op` y; return x) @?= res
  ]

opTests :: TestTree
opTests =
  testGroup
    "op Tests"
    ( opTestCases "add" (+=) 10 20 30
        ++ opTestCases "sub" (-=) 30 10 20
        ++ opTestCases "mul" (*=) 10 20 200
    )

-- (d) `while`

whileTests :: TestTree
whileTests =
  testGroup
    "while Tests"
    [ testCase "while loop" $
        def
          ( do
              x <- var 0
              while x (< 10) $ do
                x += lit 1
              return x
          )
          @?= 10
    ]

-- (e) imperative factorial

impFactorial :: Int -> Int
impFactorial n = def $ do
  result <- var 1
  i <- var n
  while i (> 0) $ do
    result *= i
    i -= lit 1
  return result

fpFactorial :: (Eq t, Num t) => t -> t
fpFactorial n = if n == 0 then 1 else n * fpFactorial (n - 1)

newtype TinyInt = TinyInt Int
  deriving (Show, Eq)

instance Arbitrary TinyInt where
  arbitrary = TinyInt <$> choose (0, 100)

impTests :: TestTree
impTests =
  testGroup
    "Imperative Programming Tests"
    [ testProperty "factorial behavior" $
        \(n :: TinyInt) -> let TinyInt i = n in impFactorial i == fpFactorial i
    ]
