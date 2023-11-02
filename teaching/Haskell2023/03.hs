-- | The countdown game

module Countdown where

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

test01 :: Wert
test01 = auswerten (App Add (Zahl 1) (Zahl 2))

test02 :: Expr
test02 =
  App
    Add
    (Zahl 7)
    (App Mul
      (App Add (Zahl 1) (Zahl 10))
      (App Add (Zahl 25) (Zahl 50))
    )

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

