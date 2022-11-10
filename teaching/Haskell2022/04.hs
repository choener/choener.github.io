
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables #-}

import Data.List (sort)
import Data.Char (isDigit)
import Text.Printf (printf)

-- * Structures for arithmetic expressions

--{{{
data Expr
  = Num Int
  | App Op Expr Expr
  deriving (Show,Eq)

data Op = Add | Sub | Mul | Div
  deriving (Show,Eq,Bounded,Enum)

newtype Value = Value {getValue :: Int}
  deriving (Show,Eq,Ord,Enum,Real,Num,Integral)
--}}}

-- | Evaluate an 'Expr' tree

value :: Expr -> Value
--{{{
value (Num k) = Value k
value (App o l r) = applyOp o (value l) (value r)
--}}}

-- | Apply a single 'Op' to values.

applyOp :: Op -> Value -> Value -> Value
--{{{
applyOp Add (Value l) (Value r) = Value (l + r)
applyOp Sub (Value l) (Value r) = Value (l - r)
applyOp Mul (Value l) (Value r) = Value (l * r)
applyOp Div (Value l) (Value r) = Value (l `div` r)
--}}}

-- | Check wether an operation would actually be legal

legal :: Op -> Value -> Value -> Bool
--{{{
legal Add l r = True
legal Sub l r = r < l
legal Mul l r = True
legal Div l r = l `mod` r == 0
--}}}

-- * Countdown (``AI'' version)

-- | Generate all possible subsequences of a list

subseqs :: [a] -> [[a]]
--{{{
subseqs [x] = [[x]]
subseqs (x:xs) = xss ++ [x] : map (x:) xss
  where xss = subseqs xs
--}}}

-- | Generate all possible expressions for our set of numbers

mkExprs :: [Int] -> [(Expr, Value)]
mkExprs = error "implement me"
