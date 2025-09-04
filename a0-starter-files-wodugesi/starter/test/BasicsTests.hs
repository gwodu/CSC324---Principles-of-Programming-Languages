module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Basics

test_power5_unit_public :: TestTree
test_power5_unit_public =
  testGroup "power5 public unit test"
  [ testCase "Simple" $ (power5 0) @?= 0
  , testCase "Simple" $ (power5 1) @?= 1
  , testCase "Simple" $ (power5 2) @?= 32
  , testCase "Simple" $ (power5 10) @?= 100000
  ]

test_ageDiscount_unit_public :: TestTree
test_ageDiscount_unit_public =
  testGroup "ageDiscount public unit tests"
  [ testCase "Young" $ (ageDiscount 10 10) @?= 8
  , testCase "Adult" $ (ageDiscount 40 10) @?= 10
  , testCase "Old" $ (ageDiscount 70 10) @?= 8
  ]

test_squareTheDiff_unit_public :: TestTree
test_squareTheDiff_unit_public =
  testGroup "squareTheDiff public unit tests"
  [ testCase "(20 - 10)^2" $ (squareTheDiff 20 10) @?= 100
  , testCase "(10 - 20)^2" $ (squareTheDiff 10 20) @?= 100
  , testCase "(20 - 0)^2" $ (squareTheDiff 20 0) @?= 400
  ]

main :: IO ()
main = defaultMain $
  testGroup "All"
  [ test_power5_unit_public
  , test_ageDiscount_unit_public
  , test_squareTheDiff_unit_public
  ]
