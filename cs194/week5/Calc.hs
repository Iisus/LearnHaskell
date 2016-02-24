{-# OPTIONS_GHC -Wall #-}

{- Calc
 -
 - https://www.seas.upenn.edu/~cis194/spring13/hw/05-type-classes.pdf
 -
 - author: Andrei Alexandru
 -}


module Calc where

import ExprT
import Parser

-- Exercise 1 --
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add le re) = eval le + eval re
eval (Mul le re) = eval le * eval re


-- Exercise 2 --
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
              Just expr -> Just (eval expr)
              Nothing   -> Nothing