-- Magic !

import Prelude as P

-- {-# Language NoImplicitPrelude #-}
-- import Prelude (error, print, Int, (<))

-- Magic !

-- printing values
-- error'ing out
-- Int, Integer, Double, etc

--{{{ fibRec
fibRec :: Int -> Int
fibRec 1 = 0
fibRec 2 = 1
fibRec x = if x < 1
           then error "needs to be positive"
           else fibRec (x-1) + fibRec (x-2)
--}}}

--{{{ fibMemo
--fibMemo :: Int -> Int
fibMemo x
  | x < 1 = error "needs to be positive"
  | otherwise = ls !! (x-1)
  where ls = 0:1:zipWith (+) ls (drop 1 ls)
--}}}

fibs = 0:1:zipWith (+) fibs (drop 1 fibs)

foo = zipWith max "hello" ['ε' .. ]

-- | Unicode infix Operatoren koennen einfach infix definiert werden
a ⊗ b = a * b

-- | und danach benutzt werden
bar = 1 ⊗ 2

--{{{ Types for fibRec, fibMemo
--fibRec :: (Eq a, Ord a, Num a) => a -> a
--fibMemo :: (Eq a, Ord a, Num a) => a -> a
--}}}



--{{{ unbekannte Funktion
unbekannt [] = []
unbekannt (x:xs) =
  let ls = [y | y <- xs, y <= x]
      rs = [y | y <- xs, y >  x]
  in  unbekannt ls ++ [x] ++ unbekannt rs
--}}}

-- Hallo, Test ... ;-)

--{{{ unbekannt, Typ
--unbekannt :: Ord a => [a] -> [a]
--}}}

