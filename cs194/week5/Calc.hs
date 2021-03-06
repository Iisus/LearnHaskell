{-# LANGUAGE FlexibleInstances #-}
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
import StackVM
import qualified Data.Map as M


-- Exercise 1 --
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add le re) = eval le + eval re
eval (ExprT.Mul le re) = eval le * eval re


-- Exercise 2 --
evalStr :: String -> Maybe Integer
evalStr str = case parseExp ExprT.Lit ExprT.Add ExprT.Mul str of
              Just expr -> Just (eval expr)
              Nothing   -> Nothing


-- Exercise 3 --
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul


-- Exercise 4 --
instance Expr Integer where
  lit = id
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


-- Exercise 5 --
instance Expr Program where
  lit x = [StackVM.PushI x]
  add x y = y ++ x ++ [StackVM.Add]
  mul x y = y ++ x ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul


-- Exercise 6 --
class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
             | Var String
             | Add VarExprT VarExprT
             | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance HasVars VarExprT where
  var = Calc.Var

instance Expr VarExprT where
  lit = Calc.Lit
  add = Calc.Add
  mul = Calc.Mul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

opMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
opMaybe f (Just x) (Just y) = Just (f x y)
opMaybe _ _ _ = Nothing


instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _     = Just x
  add lf rf m = opMaybe (+) (lf m) (rf m)
  mul lf rf m = opMaybe (*) (lf m) (rf m)

-- tests
withVars :: [(String, Integer)]
        -> (M.Map String Integer -> Maybe Integer)
        -> Maybe Integer
withVars vs expr = expr $ M.fromList vs