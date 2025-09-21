module RecursionAndDatatypes where

import Prelude hiding (filter, take)

{-------------------- Part 1: Recursion --------------------}

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

takePositive :: Int -> [Int] -> [Int]
takePositive n xs = reverse (go n xs [])
  where
    go :: Int -> [Int] -> [Int] -> [Int]
    go _ [] acc = acc                    
    go 0 _  acc = acc                    
    go c (x:xs) acc
        | x > 0    = go (c-1) xs (x:acc)
        | otherwise = go c    xs acc

{-------------------- Part 2: Datatype and Pattern Matching --------------------}

data Shape = Square Float             -- Side length s :: Float
           | Rectangle Float Float    -- Length l :: Float, Width w :: Float
           | Circle Float             -- Radius r :: Float
           | RightTriangle Float Float -- Right triangle with leg length a :: Float, b :: Float

circumference :: Shape -> Float
circumference (Square s) = 4 * s
circumference (Rectangle l w) = 2 * (l + w)
circumference (Circle r) = 2 * pi * r
circumference (RightTriangle a b) = a + b + sqrt(a*a + b*b)

data BinaryTree = Node
                | Branch BinaryTree Int BinaryTree
                deriving (Eq, Show)

invert :: BinaryTree -> BinaryTree
invert Node = Node
invert (Branch l_tree x r_tree) = Branch (invert r_tree) x (invert l_tree)

sumTree :: BinaryTree -> Int
sumTree Node = 0
sumTree (Branch l_tree x r_tree) = x + sumTree l_tree + sumTree r_tree

maxTree :: BinaryTree -> Int
maxTree Node = minBound
maxTree (Branch l_tree x r_tree) = maximum [x, maxTree l_tree, maxTree r_tree]
