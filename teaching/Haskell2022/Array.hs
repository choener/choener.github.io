
-- Kleine Einfuehrung in Arrays.
-- https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array-IArray.html

module Array where

-- Zugriff auf Arrays
import Data.Array.IArray as A

--Index-Typen in Arrays. Diese Klasse ist vorgegeben, einige ihrer Funktionen sind hier noch einmal
--preasentiert.
--
--class Ix a where
--  range :: (a, a) -> [a]
--  index :: (a, a) -> a -> Int
--  inRange :: (a, a) -> a -> Bool

-- Fuer unsere Beispiele nehmen wir 2-dimensionale Indices an.

type Dim2 = (Int,Int)

-- 'ix' sind hier die Index-Grenzen von (1,1) bis (6,6)

ix :: (Dim2,Dim2)
ix = ( (1,1), (6,6) )

-- 'rn' liefert eine Liste von allen Indices: [(1,1),(1,2),...(1,6),(2,1),...,(6,6)]

rn = range ix

-- Liefert den "linearen Index" von (2,3) gegeben die Grenzen bei (1,1),(6,6).: index ix (2,3) == 8

at = index ix (2,3)

-- testet ob (7,5) innerhalb des Indexbereichts (1,1),(6,6) liegt: nein.

ok = inRange ix (7,5)

-- | Initialisiere ein Array mit Wert=0 ueberall; an Position (2,3) wird ein Update von 0 auf 9
-- gesetzt.

test :: A.Array Dim2 Int
test = array ((1,1),(2,3)) [ (k,0) | k <- range ((1,1),(2,3)) ]
     // [ ((2,3),9) ]

-- | Boxed arrays (also Array hier) sind selbst-rekursiv; wir koennen auf das Array waehrend der
-- Konstruktion zugreifen.

acc :: A.Array Int Int
acc = array (1,6) [ (k,if k == 1 then 1 else k + acc ! (k-1))
                  | k <- [1..6] ]
