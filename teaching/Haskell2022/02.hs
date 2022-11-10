-- Magic !

{-# Language GeneralizedNewtypeDeriving #-}

import Prelude as P


f :: Maybe Bool -> Char
f _ = 'x'
f (Just False) = 'f'
f (Just True) = 't'
f Nothing = 'n'

g x = x+1

newtype Konto = Geld Int
  deriving (Num, Show, Eq, Ord)

newtype Password = Password String
  deriving (Eq)

maxx :: Ord a => a -> a -> a
maxx a b = max a b

dw :: a -> a
--dw x = x
dw = id

data Many a = IsInt Int | IsBool Bool | IsMine a

gg = sum . map (*2) . filter even

fireMissiles :: a -> IO () -- magic
fireMissiles a = do
  print "boom"

--instance Num Konto where
--  Geld a + Geld b = Geld (a+b)

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



--{{{ roller coaster
data Person = P { age :: Int, height :: Int }
guard :: Person -> Bool
guard (P a h) = not (a < 12 || h < 100)
coaster = map guard

guys = coaster [P 11 110, P 8 90, P 117 13, P 13 130]
--}}}

--{{{ make everything new
newtype Age = Age Int
newtype Height = Height Int
--}}}



--{{{ mittelwert
--scary types !
mittelwertI xs = sum xs `div` length xs
mittelwertR xs = s / l
  where s = fromIntegral (sum xs)
        l = fromIntegral (length xs)
-- whats up with mittelwertR types? magic!
--}}}



--{{{ data List
data List a = Nil | Cons a (List a)
--}}}

--{{{ summeList
summeList :: List Int -> Int
summeList Nil = 0
summeList (x `Cons` xs) = x + summeList xs
--}}}

--{{{ summeH(omeMade)
summeH :: [Int] -> Int
summeH [] = 0
summeH (x:xs) = x + summeH xs
--}}}

--{{{ summeF(old)
summeF :: [Int] -> Int
summeF = foldr (+) 0
--}}}

--{{{ summeS(um)
summeS :: [Int] -> Int
summeS = sum
--}}}


--{{{ malZwei
malZwei :: Int -> Int
malZwei x = 2 * x
--}}}

--{{{ potenz
potenz 0 x = 1
potenz k x = potenz (k-1) x * x
--}}}

