-- | The countdown game

module Countdown where

import qualified Data.Char

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


-- * Hausaufgabe
--
-- Countdown wird mit zwei Teams gegeneinander und einer Aufloesung durch die Spielleitung gespielt.
-- Um gegeneinander spielen zu koennen, brauchen wir die Moeglichkeit, unsere Eingaben zu testen.
--
-- Dabei ist es unschoen, wenn wir selbst so etwas schreiben muessen wie @App Add (Zahl 1) (Zahl
-- 2)@. Stattdessen wollen wir lieber "1+2" schreiben koennen. Die Funktion, die "1+2" nach @App Add
-- (Zahl 1) (Zahl 2)@ umbaut, nennen wir einen Parser.
--
-- Ihre Aufgabe ist, diesen Parser jetzt zu schreiben. Die Funktion unten @parse :: String -> Expr@
-- dient als Prototyp. Wobei hier @parse = undefined@ dann zu ersetzen ist.
--
-- Hier noch einige Hinweise:
--
-- 0. Fuer den Moment wollen wir annehmen das alle Eingaben korrekt sind.
-- korrekt: "(1+2)*3"
-- falsch: "(1+"
-- 1. Einen korrekten Parser zu schreiben ist kompliziert. Es ist lohnenswert, aufzuschreiben wo sie
-- "stecken bleiben", da dies genau die Punkte sind, an denen wir naechste Woche ansetzen wollen.
-- Wir wollen also als unseren Problemen und Fehlern lernen.
-- 2. "Der Weg ist das Ziel", wir sind weniger an korrekten Funktionen 'parse' interessiert, sondern
-- mehr an den "Problemen" und eventuell einer Wunschliste, a la "es waere toll wenn
-- Fehlerbehandlung gut machbar waere"
-- 3. Klammerausdruecke stellen eine Huerde da, insbesondere verschachtelte Ausdruecke:
-- "(1+(3*4))"
-- 4. Operatorrangfolge sollte beachtet werden: "2+3*4" entspricht "2+(3*4)"
-- 5. Die Funktion 'digitToInt :: Char -> Int' steht zur Verfuegung.
-- 6. Pattern Matching auf Operatoren und Klammern sollte man auch nutzen.
-- 7. Einige Funktionen habe ich vorgegeben. Diese (wie in 5.) koennen helfen, muessen aber noch
-- ausgefuellt werden.

-- | Parsed einen String zu einer 'Expr'.

-- TODO Muss vervollstaendigt werden.

parse :: String -> Expr
parse = undefined

-- | Uebersetzt "+-*/" nach 'Op'.
--
-- TODO Muss vervollstaendigt werden.

operator :: Char -> Op
operator '+' = Add
operator '-' = undefined
-- ...

-- | Uebersetzt einen String @xs@ in eine Zahl. Benutze 'digitToInt' fuer die Ziffern.

zahl :: String -> Expr
zahl xs = undefined

-- | Transformiert 'Char' nach 'Int'
-- >>> digitToInt '3' == 3

digitToInt :: Char -> Int
digitToInt = Data.Char.digitToInt

