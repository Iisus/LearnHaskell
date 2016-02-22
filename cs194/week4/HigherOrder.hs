{-# OPTIONS_GHC -Wall #-}

{- HigherOrder
 -
 - https://www.seas.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf
 -
 - author: Andrei Alexandru
 -}


module HigherOrder where


-- Exercise 1 [Wholemeal programming] --
-- 1.
fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' (x:xs)
  | even x    = (x-2) * fun1' xs
  | otherwise = fun1' xs

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

-- 2.
fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n
  | even n    = n + fun2' (n `div` 2)
  | otherwise = fun2' (3 * n + 1)

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/=1)
     . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)


-- Exercise 2 [Folding with trees] --
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)

foldTree :: [a] -> Tree a
foldTree xs = Leaf --TODO


-- Exercise 3 [More folds] --
-- 1. Implement xor
xor :: [Bool] -> Bool
xor = foldr (\x acc -> if acc then x /= acc else x) False

-- 2. Implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- 3. Implement foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (\x acc -> f acc x) base


-- Exercise 4 [Finding promes] --
