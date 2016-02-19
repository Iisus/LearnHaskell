{-# OPTIONS_GHC -Wall #-}

{- HigherOrder
 -
 - https://www.seas.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf
 -
 - author: Andrei Alexandru
 -}


module HigherOrder where


fun1' :: [Integer] -> Integer
fun1Orig [] = 1
fun1Orig (x:xs)
  | even x    = (x-2) * fun1Orig xs
  | otherwise = fun1Orig xs

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even


fun2' :: Integer -> Integer
fun2Orig 1 = 0
fun2Orig n | even n    = n + fun2Orig (n `div` 2)
           | otherwise = fun2Orig (3 * n + 1)

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/=1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)