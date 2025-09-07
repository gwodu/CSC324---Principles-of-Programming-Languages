module Basics where

{-------------------- Basic FP --------------------}
power5 :: Int -> Int
power5 n = n ^ 5

ageDiscount :: Int -> Float -> Float
ageDiscount age basePrice
    | age < 18 = basePrice * 0.8
    | age >= 60 = basePrice * 0.8
    | otherwise = basePrice * 1

squareTheDiff :: Int -> Int -> Int
squareTheDiff x y =
    let diff = x - y
    in diff ^ 2

