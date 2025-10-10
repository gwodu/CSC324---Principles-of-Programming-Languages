module HigherOrderFunctions where

import Prelude hiding (curry, uncurry)

---------------------- Part 1 (a) ----------------------

totalCost :: [Int] -> Int
totalCost prices =
    if length prices < 2
        then 0
        else sum $ map snd $ filter (\p -> snd p < fst p) $ zip (init prices) (tail prices)

---------------------- Part 1 (b) and (c) --------------

data BinaryTree a = Leaf a
                  | Branch (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Branch left val right) = Branch (mapTree f left) (f val) (mapTree f right)

transformTree :: (a -> b) -> (b -> a -> b -> b) -> BinaryTree a -> b
transformTree f g (Leaf x) = f x
transformTree f g (Branch left val right) = g (transformTree f g left) val (transformTree f g right)

preOrder :: BinaryTree a -> [a]
preOrder = transformTree (\x -> [x]) (\left val right -> [val] ++ left ++ right)

inOrder :: BinaryTree a -> [a]
inOrder = transformTree (\x -> [x]) (\left val right ->  left ++ [val] ++ right)

postOrder :: BinaryTree a -> [a]
postOrder = transformTree (\x -> [x]) (\left val right -> left ++ right ++ [val])

---------------------- Part 1 (d) ----------------------

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = f x y
