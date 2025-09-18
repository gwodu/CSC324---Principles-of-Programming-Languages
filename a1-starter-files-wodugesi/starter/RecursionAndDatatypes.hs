module RecursionAndDatatypes where

import Prelude hiding (filter, take)

{-------------------- Part 1: Recursion --------------------}

merge :: [Int] -> [Int] -> [Int]
merge xs ys = error "TODO"

takePositive :: Int -> [Int] -> [Int]
takePositive n xs = error "TODO"

{-------------------- Part 2: Datatype and Pattern Matching --------------------}

data Shape = Square Float             -- Side length s :: Float
           | Rectangle Float Float    -- Length l :: Float, Width w :: Float
           | Circle Float             -- Radius r :: Float
           | RightTriangle Float Float -- Right triangle with leg length a :: Float, b :: Float

circumference :: Shape -> Float
circumference _ = error "TODO"

data BinaryTree = Node
                | Branch BinaryTree Int BinaryTree
                deriving (Eq, Show)

invert :: BinaryTree -> BinaryTree
invert _ = error "TODO"

sumTree :: BinaryTree -> Int
sumTree _ = error "TODO"

maxTree :: BinaryTree -> Int
maxTree _ = error "TODO"
