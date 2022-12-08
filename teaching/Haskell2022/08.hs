{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables #-}
{-# Language InstanceSigs #-}
{-# Language FlexibleContexts #-}
{-# Language UndecidableInstances #-}
{-# Language FlexibleInstances #-}

-- ghc --make -O3 -rtsopts -threaded 08.hs
-- ./08 +RTS -s -N -qg -RTS

-- recursive descent parsing
-- (error logging / monad transformers)
-- parser combinators

import Data.List (sort,subsequences,(\\))
import Data.Char (isDigit,isSpace,ord)
import Text.Printf (printf)
import Control.Monad (MonadPlus(..))
import Control.Applicative (Alternative(..))
import qualified Data.Map.Lazy as Map
import qualified Debug.Trace as Debug
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Control.Parallel.Strategies as P

main :: IO ()
--{{{
main = do
  print "Give me numbers"
  ns :: [Int] <- sort . map read . words <$> getLine
  print "Give me a target"
  tgt :: Int <- fmap read getLine
  printf "Given numbers %s get closest to the target number %d with an arithmetic expression:\n" (unwords $ fmap show ns) tgt
  suggest :: String <- getLine
  let exprs = concatMap mkExprs $ subseqs ns
      (expr,nst) = nearest (Value tgt) exprs
      myexp = case runParser exprP suggest of
                Right a -> a
                Left  e -> error e
      myval = value myexp
  before <- getCurrentTime
  -- TODO what is the difference between using @length exprs@ and this?
  let eachSS :: [Int] = map (length . mkExprs) $ subseqs ns
  printf "There are %d expressions\n" $ sum (eachSS `P.using` P.parBuffer 8 P.rdeepseq)
  after <- getCurrentTime
  printf "This took %s seconds\n" (show $ diffUTCTime after before)
  printf "You have: %d\n" (getValue myval)
  printf "Optimal is: %d   %s\n" (getValue nst) (showExpr expr)
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
                  , ev <- combine l r ] -- `P.using` P.evalTraversable P.rseq
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

-- * Monadic combinator parsing

-- Define the *type* of parsers.
newtype Parser t a = Parser { parse :: [t] -> [(a,[t])] }

runParser :: (Show t, Show a) => Parser t a -> [t] -> Either String a
runParser p ts = case parse p ts of
  (a,[]):_ -> Right a
  [] -> Left "no input"
  ars -> Left $ show ars

itemP :: Parser t t
itemP = Parser go
  where go [] = []
        go (x:xs) = [(x,xs)]

atomP :: Eq t => t -> Parser t t
atomP c = Parser go
  where go [] = []
        go (x:xs) | x/=c = []
        go (x:xs) = [(x,xs)]

instance Functor (Parser t) where
  fmap :: (a -> b) -> Parser t a -> Parser t b
  fmap f (Parser p) = Parser (\cs -> [(f a,ds) | (a,ds) <- p cs])

instance Applicative (Parser t) where
  pure :: a -> Parser t a
  pure x = Parser (\cs -> [(x,cs)])
  (<*>) :: Parser t (a -> b) -> Parser t a -> Parser t b
  Parser p <*> Parser q = Parser (\cs -> [ (f a,es) | (f,ds) <- p cs
                                         , (a,es) <- q ds])

-- |
-- @
-- empty <|> p === p
-- p <|> empty === p
-- p <|> (q <|> r) === (p <|> q) <|> r
-- @

instance Applicative (Parser t) => Alternative (Parser t) where
  empty = noP
  Parser p <|> Parser q = Parser $ \cs -> p cs ++ q cs

instance (Monad (Parser t), Alternative (Parser t)) => MonadPlus (Parser t) where
  mzero = empty
  mplus = (<|>)

-- the monad laws must hold:
-- return a >>= f === f a
-- p >>= return === p
-- p >>= (\a -> (f a >>= g)) === (p >>= (\a -> f a)) >>= g
--
-- return is left and right unit for (>>=)    -- allow simplification
-- (>>=) is associative                       -- eliminate parentheses

instance Monad (Parser t) where
  return :: a -> Parser t a
  return = pure
  (>>=) :: Parser t a -> (a -> Parser t b) -> Parser t b
  Parser p >>= pq = Parser (\cs -> [ (b,es) | (a,ds) <- p cs
                                   , let Parser q = pq a
                                   , (b,es) <- q ds])

noP :: Parser t a
noP = Parser $ \cs -> []

satP :: (t -> Bool) -> Parser t t

satP c = Parser go
  where go [] = []
        go (x:xs) | c x = [(x,xs)]
        go _ = []

--satP c = itemP >>= \x -> if c x then pure x else mzero
--
--satP c = do
--  x <- itemP
--  if c x then pure x else mzero
--
--satP c = Parser goL >>= \x -> if c x then Parser (\cs -> [(x,cs)]) else Parser (\cs -> [])
--  where goL [] = []
--        goL (x:xs) = [(x,xs)]

-- fuz rho DOh : do-notation

testPP =
  itemP >>= \x1 ->
  itemP >>= \x2 ->
  itemP >>
  itemP >>= \x4 ->
  return (x1,x2,x4)

testDO = do
  x1 <- itemP
  x2 <- itemP
  itemP
  x4 <- itemP
  return (x1,x2,x4)

lineDO = do { x1 <- itemP; x2 <- itemP; itemP; x4 <- itemP; return (x1,x2,x4) }

-- recursion combinators

theseP :: Eq t => [t] -> Parser t [t]
theseP [] = pure []
theseP (t:ts) = satP (t==) >> theseP ts
--theseP = mapM_ (satP . (==))
--theseP = foldr (\t -> (>>) (satP (t ==))) (pure ())

manyP p = someP  p <|> return []
someP p = do {x <- p; xs <- manyP p; return (x:xs)}

-- example: 11123, parse '1', then parse the following '1's



-- extra

sepBy :: Parser t a -> Parser t b -> Parser t [a]
p `sepBy` s = (p `sepBy1` s) <|> return []

sepBy1 :: Parser t a -> Parser t b -> Parser t [a]
p `sepBy1` s = do {a <- p; as <- many (s >> p); return (a:as)}

bracketedP :: Parser t l -> Parser t x -> Parser t r -> Parser t x
bracketedP lP xP rP = do
  _l <- lP
  x  <- xP
  _r <- rP
  return x

chainl :: Parser t a -> Parser t (a -> a -> a) -> a -> Parser t a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser t a -> Parser t (a -> a -> a) -> Parser t a
chainl1 p op = p >>= go
  where go a = do
          f <- op
          b <- p
          go (f a b)
          <|> return a

-- lexical parsing

spaceP :: Parser Char String
spaceP = many (satP isSpace)

tokenP :: Parser Char a -> Parser Char a
tokenP p = p <* spaceP

stringP :: String -> Parser Char String
stringP = tokenP . theseP

-- finally the new expr parser, without tokenization

exprP :: Parser Char Expr
exprP = termP `chainl1` addopP

termP = factorP `chainl1` mulopP

factorP = numberP <|> bracketP

digitP :: Parser Char Int
digitP = satP isDigit >>= \x -> pure (ord x - ord '0')

numberP :: Parser Char Expr
numberP = do
  ds <- some digitP
  spaceP
  return . Num $ foldl (\acc x -> 10*acc + x) 0 ds

bracketP :: Parser Char Expr
bracketP = bracketedP l exprP r
  where l = tokenP $ atomP '('
        r = tokenP $ atomP ')'

addopP = (stringP "+" >> pure (App Add)) <|> (stringP "-" >> pure (App Sub))
mulopP = (stringP "*" >> pure (App Mul)) <|> (stringP "/" >> pure (App Div))

-- * Memoisation

-- fixpoint combinator
fix :: (a -> a) -> a
fix f = let x = f x in x

-- Nicht selbst-rekursive fibonacci Zahlen
-- auch "open recursion"
fib :: (Int -> Int) -> Int -> Int
fib f 0 = 1
fib f 1 = 1 -- Debug.traceShow 1 1
fib f n = f (n-1) + f (n-2)

-- | fix macht fib korrekt rekursiv
runfib :: Int -> Int
runfib = fix fib

bla = fib bla

memoList :: [Int] -> (Int -> a) -> (Int -> a)
memoList ks f = (map f ks !!)

memofib :: Int -> Int
memofib = fix (memoList [0..1000] . fib)

memofib' :: Int -> Int
memofib' = fix ((\f -> (map f [0..] !!)) . fib)

memoSeqs :: forall a . [[Int]] -> ([Int] -> a) -> ([Int] -> a)
memoSeqs sqs f = (tbl Map.!)
  where
    tbl :: Map.Map [Int] a
    tbl = Map.fromList [ (s,f s) | s <- sqs ]

-- |
-- length . concatMap mkExprs $ subseqs [1..6]
-- 5341067
-- (6.55 secs, 6,155,652,240 bytes)
--
-- length . concatMap (fix orExprs) $ subseqs [1..6]
-- 5341067
-- (6.60 secs, 6,182,532,112 bytes)
--
-- length . concatMap (fix (orExprs . memoSeqs (subseqs [1..6]))) $ subseqs [1..6]
-- 5341067
-- (4.45 secs, 4,110,812,488 bytes)
-- length . concatMap recExprs $ subseqs [1..6]
-- 5341067
-- (6.68 secs, 6,182,532,000 bytes)

recExprs :: [Int] -> [(Expr,Value)]
recExprs = orExprs recExprs

orExprs :: ([Int] -> [(Expr,Value)]) -> [Int] -> [(Expr, Value)]
orExprs mk [x] = [(Num x, Value x)]
orExprs mk xs =
  [ ev | (ys,zs) <- unmerges xs
  , l <- mk ys, r <- mk zs
  , ev <- combine l r ]



showExpr :: Expr -> String
showExpr (Num k) = show k
showExpr (App op l r) = printf "(%s%s%s)" (showExpr l) (showOp op) (showExpr r)

showOp :: Op -> String
showOp Add = "+"
showOp Sub = "-"
showOp Mul = "*"
showOp Div = "/"
