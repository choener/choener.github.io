
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables #-}

import Data.List (sort,subsequences,(\\))
import Data.Char (isDigit)
import Text.Printf (printf)

main :: IO ()
--{{{
main = do
  print "Give me numbers"
  ns :: [Int] <- sort . map read . words <$> getLine
  print "Give me a target"
  tgt :: Int <- fmap read getLine
  printf "Given numbers %s get closest to the target number %d with an arithmetic expression:\n" (unwords $ fmap show ns) tgt
  suggest :: String <- getLine
  let (expr,nst) = nearest (Value tgt) . concatMap mkExprs $ subseqs ns
      myexp = token2Expr $ tokenize suggest
      myval = value myexp
  printf "You have: %d\n" (getValue myval)
  printf "Optimal is: %d\n" (getValue nst)
--}}}

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
--{{{
mkExprs [x] = [(Num x, Value x)]
mkExprs xs = [ ev | (ys,zs) <- unmerges xs
                  , l <- mkExprs ys, r <- mkExprs zs
                  , ev <- combine l r ]
--}}}

-- | Merge two sorted lists. Not needed!

merge :: Ord a => [a] -> [a] -> [a]
--{{{
merge [] rs = rs
merge ls [] = ls
merge (l:ls) (r:rs)
  | l <= r = l : merge ls (r:rs)
  | r < l  = r : merge (l:ls) rs
--}}}

-- | Given ordered @xs@, create two ordered sublists, which when merged are equal to @xs@.
--
-- let (ys,zs) = unmerges xs
-- ∀ y in ys . z in zs : merge y z == xs

unmerges :: Show a => [a] -> [([a],[a])]
--{{{
unmerges [x,y] = [([x],[y]), ([y],[x])]
unmerges (x:xs) = [([x],xs),(xs,[x])] ++ concatMap (add x) (unmerges xs)
  where add x (ys,zs) = [(x:ys,zs),(ys,x:zs)]
unmerges burn = error $ show burn
--}}}

--{{{ alternative notation of 'unmerges'
altunmerges xs = [ (ls,rs) | ls <- subsequences xs, not (null ls)
                           , let rs = xs \\ ls, not (null rs)
                           ]
qqq xs = sort (altunmerges xs) == sort (unmerges xs)
--}}}

-- | Combine two expression trees with all possible, legal operations

combine :: (Expr,Value) -> (Expr,Value) -> [(Expr,Value)]
--{{{
combine (l,v) (r,w) = [(App op l r, applyOp op v w) | op <- ops, legal op v w]
  where ops = [Add,Sub,Mul,Div] -- [minBound..maxBound]
--}}}

-- | Find the solution nearest to the target, break early if exact solution found

nearest :: (Eq b, Ord b, Num b) => b -> [(a,b)] -> (a,b)
--{{{
nearest n ((e,v):evs)
  | d == 0 = (e,v)
  | otherwise = search n d (e,v) evs
  where d = abs (n-v)
--}}}

-- | Actual search function for the nearest candidate solution

search :: (Ord a, Num a) => a -> a -> (b,a) -> [(b,a)] -> (b,a)
--{{{
search n d ev [] = ev
search n d ev ((e,v):evs)
  | d' == 0 = (e,v)
  | d' < d  = search n d' (e,v) evs
  | d' >= d = search n d ev evs
  where d' = abs (n-v)
--}}}

-- * Tokenization

data Token
--{{{
  = TNum Int | TOp Op | TLeft | TRight
  deriving (Show,Eq)
--}}}

-- * Turn the input into tokens, these are more easy to handle

tokenize :: String -> [Token]
--{{{
tokenize [] = []
tokenize (x:xs)
  | isDigit x = let (ls,rs) = span isDigit xs
                in  TNum (read $ x:ls) : tokenize rs
  | x `elem` "+-*/" = TOp (parseOp x) : tokenize xs
  | x == '(' = TLeft : tokenize xs
  | x == ')' = TRight : tokenize xs

parseOp :: Char -> Op
parseOp '+' = Add
parseOp '-' = Sub
parseOp '*' = Mul
parseOp '/' = Div
--}}}

-- | Given user input as a string, say @(1+2)*3 - (8/4)@ return the correct 'Expr'.
--
-- This version performs no error checking of any kind.

-- turn into expr, by following order of precedence of operations with splitting!

token2Expr :: [Token] -> Expr
--{{{
token2Expr xs = case pSumPNP xs
  of Just (expr,[]) -> expr
     Nothing -> error $ show xs
--}}}

-- | Parse Numbers and parentheses with highest priority

pNumParen :: [Token] -> Maybe (Expr,[Token])
--{{{
pNumParen (TNum n:xs) = Just (Num n, xs)
pNumParen (TLeft:xs) = case pSumPNP xs of
  Just (expr, TRight:ys) -> Just (expr, ys)
  Just _ -> Nothing -- misses clothing bracket
  Nothing -> Nothing
pNumParen _ = Nothing
--}}}

-- | Parse products with next highest priority

pProdNP :: [Token] -> Maybe (Expr,[Token])
--{{{
pProdNP xs = case pNumParen xs of
  Just (el, TOp Mul:ys) -> case pProdNP ys of
    Just (er, zs) -> Just (App Mul el er, zs)
    Nothing -> Nothing
  Just (el, TOp Div:ys) -> case pProdNP ys of
    Just (er, zs) -> Just (App Div el er, zs)
    Nothing -> Nothing
  res -> res
--}}}

-- | Parse sums with lowest priority

pSumPNP :: [Token] -> Maybe (Expr,[Token])
--{{{
pSumPNP xs = case pProdNP xs of
  Just (el, TOp Add:ys) -> case pSumPNP ys of
    Just (er, zs) -> Just (App Add el er, zs)
    Nothing -> Nothing
  Just (el, TOp Sub:ys) -> case pSumPNP ys of
    Just (er, zs) -> Just (App Sub el er, zs)
    Nothing -> Nothing
  res -> res
--}}}


