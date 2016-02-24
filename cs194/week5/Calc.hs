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


-- Exercise 3 --
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul


-- Exercise 4 --
instance Expr Integer where
  lit x = x
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (0<)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = lit (max x y)
  mul (MinMax x) (MinMax y) = lit (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = lit (x+y)
  mul (Mod7 x) (Mod7 y) = lit (x*y)

-- tests
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMinMax  = testExp :: Maybe MinMax
testMod7    = testExp :: Maybe Mod7
