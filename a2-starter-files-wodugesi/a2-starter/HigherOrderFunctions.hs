module HigherOrderFunctions where

import Prelude hiding (curry, uncurry)

---------------------- Part 1 (a) ----------------------

totalCost :: [Int] -> Int
totalCost prices = error "TODO"

---------------------- Part 1 (b) and (c) --------------

data BinaryTree a = Leaf a
                  | Branch (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f tree = error "TDOO"

transformTree :: (a -> b) -> (b -> a -> b -> b) -> BinaryTree a -> b
transformTree f g tree = error "TODO"

preOrder :: BinaryTree a -> [a]
preOrder = error "TODO"

inOrder :: BinaryTree a -> [a]
inOrder = error "TODO"

postOrder :: BinaryTree a -> [a]
postOrder = error "TODO"

---------------------- Part 1 (d) ----------------------

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = error "TODO"

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = error "TODO"
