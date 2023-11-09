

{-# Language ScopedTypeVariables #-}

import Data.List (sort,subsequences,(\\))
import Data.Char (isDigit)
import Text.Printf (printf)

main :: IO ()
main = do
  print "Give me numbers"
  ns :: [Int] <- sort . map read . words <$> getLine
  print "Give me a target"
  tgt :: Int <- fmap read getLine
  printf "Given numbers %s get closest to the target number %d with an arithmetic expression:\n" (unwords $ fmap show ns) tgt
  suggest :: String <- getLine
  let (expr,nst) = nearest tgt . concatMap mkExprs $ subseqs ns
      myexp = token2Expr $ tokenize suggest
  print myexp
  printf "You have: %d\n" $ auswerten myexp
  print expr
  printf "Optimal is: %d\n" nst



-- | Datentyp for die legalen Operatoren

data Op = Add | Sub | Mul | Div
  deriving (Show,Eq,Read,Bounded,Enum)

-- | Die Form von Countdown Ausdruecken

data Expr
  = Zahl Int
  | App Op Expr Expr
  deriving (Show,Read,Eq)

-- | Alias fuer den Wert eines Countdowns

type Wert = Int

-- | Gegeben Op und zwei Werte, rechne deren Ergebnis

appOp :: Op -> Wert -> Wert -> Wert
appOp Add l r = l + r
appOp Sub l r = l - r
appOp Mul l r = l * r
appOp Div l r = l `div` r

-- | Ausdruck auswerten

auswerten :: Expr -> Wert
auswerten (Zahl k) = k
auswerten (App o l r) = appOp o (auswerten l) (auswerten r)

-- | Ist eine Operation legal?

legal :: Op -> Wert -> Wert -> Bool
legal Add l r = True
legal Sub l r = r < l
legal Mul l r = True
legal Div l r = l `mod` r == 0

-- * Countdown (``AI'' version)

-- | All moeglichen Teilsequenzen einer Liste

subseqs :: [a] -> [[a]]
subseqs [x] = [[x]]
subseqs (x:xs) = xss ++ [x] : map (x:) xss
  where xss = subseqs xs

-- | Given ordered @xs@, create two ordered sublists, which when merged are equal to @xs@.
--
-- let (ys,zs) = unmerges xs
-- forall y in ys . z in zs : merge y z == xs

unmerges :: Show a => [a] -> [([a],[a])]
unmerges [x,y] = [([x],[y]), ([y],[x])]
unmerges (x:xs) = [([x],xs),(xs,[x])] ++ concatMap (add x) (unmerges xs)
  where add x (ys,zs) = [(x:ys,zs),(ys,x:zs)]
unmerges burn = error $ show burn


-- | Combine two expression trees with all possible, legal operations

combine :: (Expr,Int) -> (Expr,Int) -> [(Expr,Int)]
combine (l,v) (r,w) = [(App op l r, appOp op v w) | op <- ops, legal op v w]
  where ops = [Add,Sub,Mul,Div] -- [minBound..maxBound]

-- | Generate all possible expressions for our set of numbers

mkExprs :: [Int] -> [(Expr, Int)]
mkExprs [x] = [(Zahl x, x)]
mkExprs xs = [ ev | (ys,zs) <- unmerges xs
                  , l <- mkExprs ys, r <- mkExprs zs
                  , ev <- combine l r ]


nearest :: Int -> [(Expr,Int)] -> (Expr,Int)
nearest n ((e,v):evs)
-- direkt eine Loesung gefunden?
  | d == 0 = (e,v)
-- nein? Suche starten, mit Abstand d
  | otherwise = search n d (e,v) evs
  where d = abs (n-v)

search :: Int -> Int -> (Expr,Int) -> [(Expr,Int)] -> (Expr,Int)
-- es gibt nur suboptimale Lsg
search n d ev [] = ev
-- teste naechste Lsg
search n d ev ((e,v):evs)
-- optimal
  | d' == 0 = (e,v)
-- besser
  | d' < d  = search n d' (e,v) evs
-- schlechter
  | d' >= d = search n d ev evs
  where d' = abs (n-v)



-- * Tokenization

data Token
  = TNum Int | TOp Op | TLeft | TRight
  deriving (Show,Eq)

-- * Turn the input into tokens, these are more easy to handle

tokenize :: String -> [Token]
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

-- | Given user input as a string, say @(1+2)*3 - (8/4)@ return the correct 'Expr'.
--
-- This version performs no error checking of any kind.

-- turn into expr, by following order of precedence of operations with splitting!

token2Expr :: [Token] -> Expr
token2Expr xs = case pSumPNP xs
  of Just (expr,[]) -> expr
     Nothing -> error $ show xs

-- | Parse Numbers and parentheses with highest priority

pNumParen :: [Token] -> Maybe (Expr,[Token])
pNumParen (TNum n:xs) = Just (Zahl n, xs)
pNumParen (TLeft:xs) = case pSumPNP xs of
  Just (expr, TRight:ys) -> Just (expr, ys)
  Just _ -> Nothing -- misses clothing bracket
  Nothing -> Nothing
pNumParen _ = Nothing

-- | Parse products with next highest priority

pProdNP :: [Token] -> Maybe (Expr,[Token])
pProdNP xs = case pNumParen xs of
  Just (el, TOp Mul:ys) -> case pProdNP ys of
    Just (er, zs) -> Just (App Mul el er, zs)
    Nothing -> Nothing
  Just (el, TOp Div:ys) -> case pProdNP ys of
    Just (er, zs) -> Just (App Div el er, zs)
    Nothing -> Nothing
  res -> res

-- | Parse sums with lowest priority

pSumPNP :: [Token] -> Maybe (Expr,[Token])
pSumPNP xs = case pProdNP xs of
  Just (el, TOp Add:ys) -> case pSumPNP ys of
    Just (er, zs) -> Just (App Add el er, zs)
    Nothing -> Nothing
  Just (el, TOp Sub:ys) -> case pSumPNP ys of
    Just (er, zs) -> Just (App Sub el er, zs)
    Nothing -> Nothing
  res -> res

