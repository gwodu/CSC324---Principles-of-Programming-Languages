{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import RecursionAndDatatypes

test_merge_unit_public :: TestTree
test_merge_unit_public =
  testGroup "merge public unit tests"
  [ testCase "[1, 3, 5], [2, 4, 6]" $ (merge [1, 3, 5] [2, 4, 6]) @?= [1, 2, 3, 4, 5, 6]
  , testCase "[1, 2, 3], [4, 5, 6]" $ (merge [1, 2, 3] [4, 5, 6]) @?= [1, 2, 3, 4, 5, 6]
  , testCase "[2, 4, 6], [1, 3, 5]" $ (merge [2, 4, 6] [1, 3, 5]) @?= [1, 2, 3, 4, 5, 6]
  ]

test_takePositive_unit_public :: TestTree
test_takePositive_unit_public =
  testGroup "takePositive public unit tests"
  [ testCase "takePositive 10 []"             $ takePositive 10 [] @?= []
  , testCase "takePositive 1 [1, -2, 3, -4]"  $ takePositive 1 [1, -2, 3, -4] @?= [1]
  , testCase "takePositive 10 [1, -2, 3, -4]" $ takePositive 10 [1, -2, 3, -4] @?= [1, 3]
  ]

test_circumference_unit_public :: TestTree
test_circumference_unit_public =
  testGroup "circumference public unit tests"
  [ testCase "Square"    $ circumference (Square 6) @?= 24
  , testCase "Rectangle" $ circumference (Rectangle 3 5) @?= 16
  , testCase "Circle"    $ circumference (Circle 5) @?= 2 * 5 * pi
  , testCase "Triangle"  $ circumference (RightTriangle 3 4) @?= 12
  ]

leaf = Node
zigzag = Branch (Branch Node 2 (Branch (Branch Node 4 Node) 3 Node)) 1 Node
zagzig = Branch Node 1 (Branch (Branch Node 3 (Branch Node 4 Node)) 2 Node)

test_invert_unit_public :: TestTree
test_invert_unit_public =
  testGroup "invert public unit tests"
  [ testCase "Leaf"   $ invert leaf @?= leaf
  , testCase "Small"  $ invert (Branch (Branch Node 1 Node) 2 Node) @?= (Branch Node 2 (Branch Node 1 Node))
  , testCase "Zigzag" $ invert zigzag @?= zagzig
  , testCase "Zagzig" $ invert zagzig @?= zigzag
  ]

test_sumTree_unit_public :: TestTree
test_sumTree_unit_public =
  testGroup "sumTree public unit tests"
  [ testCase "Leaf"   $ sumTree leaf @?= 0
  , testCase "Small"  $ sumTree (Branch (Branch Node 1 Node) 2 Node) @?= 3
  , testCase "Zigzag" $ sumTree zigzag @?= 10
  ]

test_maxTree_unit_public :: TestTree
test_maxTree_unit_public =
  testGroup "maxTree public unit test"
  [ testCase "Leaf"   $ maxTree leaf @?= (minBound :: Int)
  , testCase "Small"  $ maxTree (Branch (Branch Node 1 Node) 2 Node) @?= 2
  , testCase "Zigzag" $ maxTree zigzag @?= 4
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ test_merge_unit_public
  , test_takePositive_unit_public
  , test_circumference_unit_public
  , test_invert_unit_public
  , test_sumTree_unit_public
  , test_maxTree_unit_public
  ]
