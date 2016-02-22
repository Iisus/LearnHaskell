{-# OPTIONS_GHC -Wall #-}

{- HigherOrder
 -
 - https://www.seas.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf
 -
 - author: Andrei Alexandru
 -}


module HigherOrder where

import Data.List

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
  deriving (Show, Eq)

treeH :: Tree a -> Integer
treeH Leaf = 0
treeH (Node h _ _ _) = h

insertInTree :: a -> Tree a -> Tree a
insertInTree x Leaf = Node 0 Leaf x Leaf
insertInTree x (Node _ t1 x' t2)
  | h1 < h2   = Node (h2 + 1) (insertInTree x t1) x' t2
  | otherwise = Node (h1 + 1) t1 x' (insertInTree x t2)
  where h1 = treeH t1
        h2 = treeH t2

foldTree :: [a] -> Tree a
foldTree = foldr insertInTree Leaf


-- Exercise 3 [More folds] --
-- 1. Implement xor
xor :: [Bool] -> Bool
xor = foldr (\x acc -> if acc then x /= acc else x) False

-- 2. Implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- 3. Implement foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse


-- Exercise 4 [Finding primes] --
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundarm :: Integer -> [Integer]
sieveSundarm n = map ((+1) . (*2)) $ [1..n] \\
               [i+j + 2*i*j | (i, j) <- cartProd [1..n] [1..n],
                                       i<=j, (i+j + 2*i*j)<=n]

sieveSundarm' :: Integer -> [Integer]
sieveSundarm' n = map ((+1) . (*2)) $ [1..n] \\ (filter (<=n) .
               map (\(i, j) -> i+j + 2*i*j) . filter (uncurry (<=))
               $ cartProd [1..n] [1..n])
