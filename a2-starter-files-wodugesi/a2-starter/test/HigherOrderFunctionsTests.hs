module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (curry, uncurry)

import HigherOrderFunctions

test_totalCost_unit_public :: TestTree
test_totalCost_unit_public =
  testGroup "totalCost public unit tests"
  [ testCase "Simple 1" $ totalCost [1, 2, 3] @?= 0
  , testCase "Simple 2" $ totalCost [3, 2, 1] @?= 3
  , testCase "Up and down 1" $ totalCost [1, 2, 1, 2, 1, 2, 1, 2] @?= 3
  , testCase "Up and down 2" $ totalCost [3, 2, 1, 2, 3, 2, 1, 2, 3] @?= 6
  ]

sorted :: BinaryTree Int
sorted =
  Branch
    (Branch
      (Leaf 1)
      2
      (Leaf 3))
    4
    (Branch
      (Leaf 5)
      6
      (Leaf 7))

treeInt :: BinaryTree Int
treeInt =
  Branch
    (Branch
      (Leaf 7)
      (-3)
      (Leaf 12))
    5
    (Branch
      (Leaf (-8))
      0
      (Leaf 9))

test_mapTree_unit_public :: TestTree
test_mapTree_unit_public =
  testGroup "mapTree public unit tests"
  [ testCase "Do nothing"   $ mapTree id treeInt @?=
    Branch (Branch (Leaf 7) (-3) (Leaf 12)) 5 (Branch (Leaf (-8)) 0 (Leaf 9))
  , testCase "Double"       $ mapTree (*2) treeInt @?=
    Branch (Branch (Leaf 14) (-6) (Leaf 24)) 10 (Branch (Leaf (-16)) 0 (Leaf 18))
  , testCase "Operations"   $ mapTree ((*2) . (+3)) treeInt @?=
    (mapTree (*2)           $ mapTree (+3) treeInt)
  , testCase "Non-negative" $ mapTree (>=0) treeInt @?=
    Branch (Branch (Leaf True) (False) (Leaf True)) True (Branch (Leaf (False)) True (Leaf True))
  ]

test_tree_unit_public :: TestTree
test_tree_unit_public =
  testGroup "transformTree public unit tests"
  [ testCase "Sum"       $ transformTree id (\x y z -> x + y + z) sorted @?= 28
  , testCase "preOrder"  $ preOrder sorted @?= [4, 2, 1, 3, 6, 5, 7]
  , testCase "inOrder"   $ inOrder sorted @?= [1, 2, 3, 4, 5, 6, 7]
  , testCase "postOrder" $ postOrder sorted @?= [1, 3, 2, 5, 7, 6, 4]
  ]

test_curry_unit_public :: TestTree
test_curry_unit_public =
  testGroup "Curry public unit tests"
  [ testCase "+" $ uncurry (+) (1, 2) @?= 3
  , testCase "*" $ uncurry (*) (42, 24) @?= 1008
  , testCase "pi1" $ curry fst 3 4 @?= 3
  , testCase "pi2" $ curry snd 5 6 @?= 6
  ]

main :: IO ()
main = defaultMain $
  testGroup "All tests"
  [ test_totalCost_unit_public
  , test_mapTree_unit_public
  , test_tree_unit_public
  , test_curry_unit_public
  ]
