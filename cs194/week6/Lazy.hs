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

-- Exercise 3 --
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show stream = show . take 20 $ streamToList stream

-- Exercise 4 --
streamRepeat :: a -> Stream a
streamRepeat x = (Cons x (streamRepeat x))

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

-- Exercise 5 --
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x1 xs1) s2 = Cons x1 (interleaveStreams s2 xs1)

ruler' :: Integer -> Stream Integer
ruler' x = interleaveStreams (streamRepeat x) (ruler' (x+1))

ruler :: Stream Integer
ruler = ruler' 0