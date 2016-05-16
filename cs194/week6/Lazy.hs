{-# OPTIONS_GHC -Wall #-}

{- Calc
 -
 - http://www.seas.upenn.edu/~cis194/spring13/hw/06-laziness.pdf
 -
 - author: Andrei Alexandru
 -}


module Lazy where

-- Exercise 1 --
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 --
fib' :: Integer -> Integer -> [Integer]
fib' a b = a : fib' b (a+b)

fibs2 :: [Integer]
fibs2 = fib' 0 1
