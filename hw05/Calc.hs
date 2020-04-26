{-# LANGUAGE FlexibleInstances #-}

module Calc where
import qualified Data.Map as M
import ExprT
import Parser

-- ex 1
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- ex 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . (parseExp Lit Add Mul)

-- ex 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- ex 4
newtype MinMax = MinMax Integer deriving (Show, Eq)
newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit = Mod7 . flip mod 7
  add (Mod7 x) (Mod7 y) = Mod7 . flip mod 7 $ x + y
  mul (Mod7 x) (Mod7 y) = Mod7 . flip mod 7 $ x * y

-- ex 6
class HasVars a where
  var :: String -> a

data VarExprT = Lit' Integer
              | Add' VarExprT VarExprT
              | Mul' VarExprT VarExprT
              | Var String

instance Expr VarExprT where
  lit = Lit'
  add = Add'
  mul = Mul'

instance HasVars VarExprT where
  var = Var

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = Just . (const x)
  add f g m = liftMaybe2 (+) (f m) (g m)
  mul f g m = liftMaybe2 (*) (f m) (g m)
  -- add f g m = pure (+) <*> (f m) <*> (g m)
  -- mul f g m = pure (*) <*> (f m) <*> (g m)
  -- add f g m = CM.liftM2 (+) (f m) (g m)
  -- mul f g m = CM.liftM2 (*) (f m) (g m)

liftMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe2 f (Just x) (Just y) = Just $ f x y
liftMaybe2 f _ _ = Nothing

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
