{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

-- | Mittels
-- @
-- ghc -j -O3 -fllvm -optlo-O3 -rtsopts -threaded -ddump-to-file -ddump-simpl -dsuppress-all  -i 11.hs
-- @
-- kann man den Intermediaer-code generieren und anschauen.

module Eleven where

import qualified Prelude
import Prelude hiding (sum, concatMap, Maybe(..), filter, map)

-- Striktes Tupel. TypeOperators erlaubt Infix-Datentypen.

data a :!: b = !a :!: !b

-- Striktes Maybe

data Maybe a = Nothing | Just !a

-- Ein Schritt in einem Stream (Schleife)

data Step a s = Yield a !s | Skip !s | Done



-- * Expliziter State @s@. Das wird sich als schlechte Idee erweisen.

-- | Erst einmal Streams definieren als Funktion von @s@ nach @Step@. Zusaetzlich wird das aktuelle
-- @s@ gespeichert.

data StreamNo a s = StreamNo !(s -> Step a s) !s

-- | Definition von 'sumNo'. 'sumNo' ist rekursiv und wird solange via @next s@ neue Steps
-- produzieren bis @Done@ erreicht ist. dabei wird @a@ jeweils erhoeht.

sumNo :: Num a => StreamNo a s -> a
{-# Inline [0] sumNo #-}
sumNo (StreamNo next s0) = go 0 s0
  where
    go !a !s = case next s of
      Done -> a
      Skip t -> go a t
      Yield x t -> go (a+x) t

-- | 'enumNo' enumeriert Streams von [i..j]. Beachte das hier nichts rekursiv ist. Der Stream hat
-- eine Funktion @go@ die sagt wie man von @s@ nach @s+1@ kommt; sonst nichts.

enumNo :: Int -> Int -> StreamNo Int Int
{-# Inline [0] enumNo #-}
enumNo l h = StreamNo go l
  where
    go !k | k > h = Done
          | otherwise = Yield k (k+1)

-- Alles ok? Prinzipiell funktioniert unsere Definition.

summeNo l h = sumNo $ enumNo l h

-- Nein, Manipulation des Streams! Bugs! Das man State @s@ des Streams aendern kann, ist so nicht
-- gewollt. Man kann so leicht Bugs produzieren, waehrend man eigentlich (f x = x+10) wollte.

summeNONONO l h = sumNo . rly $ enumNo l h
  where rly (StreamNo f s) = StreamNo f (s+10)



-- * Existential Quantification

-- | Dadurch das @s@ existentiell quantifiziert ist, kann nur die Funktion die den Stream und die
-- Funktion definiert, @s@ manipulieren. Alle anderen Funktionen koennen in @Stream f s@ nur @f s@
-- nutzen, nicht @s@ manipulieren.

data Stream a = forall s . Stream !(s -> Step a s) !s

-- | 'sum' ist rekursiv definiert. Da wir @next@ nicht selbst definiert haben, koennen wir @next@
-- nur auf die jeweiligen @s@ anwenden. Das Resultat @Done, Skip, Yield@ koennen wir natuerlich
-- nutzen. Aber die @t@ sind auch wieder tabu, koennen nur an @next@ gefuettert werden.

sum :: Num a => Stream a -> a
{-# Inline [0] sum #-}
sum (Stream next s0) = go 0 s0
  where
    go !a !s = case next s of
      Done -> a
      Skip !t -> go a t
      Yield !x !t -> go (a+x) t

-- | Hier wird die Stream-funktion @go@ zusammen mit dem initialen State @l@ erstellt. Da wir @go@
-- selbst schreiben duerfen wir @k@ manipulieren wie wir wollen; aber niemand anders!

enum :: Int -> Int -> Stream Int
{-# Inline [0] enum #-}
enum l h = Stream go l
  where
    go !k | k > h = Done
          | otherwise = Yield k (k+1)

-- | Ersetzt alle @Yield x t@ durch @Skip t@, wenn @f x == False@. Die Alternative
-- @
-- Yield !x !t | f x -> Yield x t
--   | otherwise = go (next t)
-- @
-- ist rekursiv und produziert schlechten Code!
--
-- (Ein Hinweis: das ist nicht mehr "ganz wahr", da Haskell mittlerweile besseren Code produziert,
-- aber fuer uns ist es wahr genug).

filter :: (a -> Bool) -> Stream a -> Stream a
{-# Inline [0] filter #-}
filter f (Stream next s0) = Stream go s0
  where
    go !s = case next s of
      Done -> Done
      Skip !t -> Skip t
      Yield !x !t | f x -> Yield x t
        | otherwise -> Skip t

-- | Code testen.

summe :: Int -> Int -> Int
{-# NoInline summe #-}
summe l h = sum . filter even $ enum l h

-- Kompiliert nicht! Keine Chance "x" zu aendern. 'enum' kann sein @k@ aendern, aber nicht andere
-- @k@.

--summeYES l h = sum . rly $ enum l h
--  where rly (Stream f x) = Stream f (x+10)

-- | concatMap erlaubt es Streams zu verschachteln und ist aequivalent zu
-- @ concat [f i | i <- [1..10]] @ mit @ f j = [j..10] @

concatMap :: (a -> Stream b) -> Stream a -> Stream b
{-# Inline [0] concatMap #-}
concatMap f (Stream next0 s0) =  Stream next (s0 :!: Nothing)
  where
    {-# INLINE next #-}
    next (!s :!: Nothing) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip (s' :!: Nothing)
      Yield x s' -> Skip (s' :!: Just (f x))

    next (!s :!: Just (Stream g t)) = case g t of
      Done       -> Skip    (s :!: Nothing)
      Skip    t' -> Skip    (s :!: Just (Stream g t'))
      Yield x t' -> Yield x (s :!: Just (Stream g t'))

-- | Test von concatMap

sumij :: Int -> Int -> Int
{-# NoInline sumij #-}
sumij i j = sum . filter even . concatMap (`enum` 10) $ enum i j


-- | Definiert @map@ fuer Streams.

map :: (a -> b) -> Stream a -> Stream b
{-# Inline [0] map #-}
map f (Stream next s0) = Stream go s0
  where
    go !s = case next s of
      Done -> Done
      Skip !t -> Skip t
      Yield !x !t -> Yield (f x) t

-- | Test des Ganzen

mapsumsquare :: Int -> Int -> Int
{-# NoInline mapsumsquare #-}
mapsumsquare i j = sum . map (\x -> x*x) $ enum i j
