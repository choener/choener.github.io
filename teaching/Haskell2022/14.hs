{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | In diesem Module werden InPlace Operationen, Datenstrukturen, und Algorithmen angesehen.
--
-- Vollstaendige externe libraries: primitive, vector
--
-- Warnung: deeply magical

-- ST, RealWorld, Referential Transparency (and how to destroy it)
-- PrimArray, Vector, Haskell's onboard Arrays
-- freeze vs unsafeFreeze, thaw vs unsafeThaw
-- (show how this breaks referential transparency)
-- coerce, unsafeCoerce

module InPlace where

import GHC.Exts (RealWorld(..))
import Control.Monad.ST
import qualified Data.Primitive.PrimArray as PA
import Control.Monad
import Data.STRef
import Data.Primitive (PrimArray, MutablePrimArray)
import Data.Primitive.Types
import Debug.Trace
import qualified Control.Monad.State.Strict as S
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM



-- * Beispiele fuer sinnvolle Nutzung von ST

-- | Modifizierbarer Accumulator

sumST :: Num a => [a] -> a
sumST xs = runST $ do
  acc <- newSTRef 0
  forM_ xs $ modifySTRef' acc . (+)
  readSTRef acc

-- | Fibonacci (yet again!)

fibST :: Integer -> Integer
fibST n
  | n<2 = n
  | otherwise = runST $ do
      l <- newSTRef 0
      r <- newSTRef 1
      go n l r
  where
    go :: Integer -> STRef s Integer -> STRef s Integer -> ST s Integer
    go 0 now _ = readSTRef now
    -- Achtung! Hier spielt jetzt die Reihenfolge eine grosse Rolle.
    go n now next = do
      prev <- readSTRef now
      here <- readSTRef next
      writeSTRef now here
      -- ($!) macht die rhs strikt, kurz thunks besprechen!
      writeSTRef next $! prev + here
      go (n-1) now next

-- * Sortieren

-- | "quicksort" mit einfachem pivot

funqs :: Ord a => [a] -> [a]
funqs [] = []
funqs (pivot:rest) =
  -- kopiert jeweils *alle* Elemente, linearer Extra
  -- Speicheraufwand
  let smaller = funqs [a | a <- rest, a<=pivot]
      larger  = funqs [a | a <- rest, a> pivot]
  in smaller ++ [pivot] ++ larger

-- | Swap zweier Werte in einem PrimArray

swapPA :: Prim a => MutablePrimArray s a -> Int -> Int -> ST s ()
swapPA pa l r = do
  x <- PA.readPrimArray pa l
  y <- PA.readPrimArray pa r
  PA.writePrimArray pa l y
  PA.writePrimArray pa r x

-- | Array, pivot, momentaner pivot, idx der zu vergleichen ist

loopPA :: (Ord a, Prim a) => MutablePrimArray s a -> a -> Int -> Int -> ST s Int
loopPA pa pivot slot i = do
  -- neues Element
  val <- PA.readPrimArray pa i
  if val < pivot
  -- ist kleiner pivot, also i und slot vertauschen
  -- neuer slot muss dann groesser werden
  then swapPA pa i slot >> return (slot+1)
  -- gleichen slot behalten
  else return slot

-- | Gegeben array und bounds, ordner Elemente um das pivot element

partPA :: (Show a, Ord a, Prim a) => MutablePrimArray s a -> Int -> Int -> Int -> ST s Int
partPA pa begin end pidx = do
  -- waehle pivot
  pivot <- PA.readPrimArray pa pidx
  swapPA pa pidx end
  -- fuer alle begin..end-1
  slot <- foldM (loopPA pa pivot) begin [begin..end-1]
  swapPA pa slot end >> return slot

stQuickSort :: (Show a, Ord a, Prim a) => MutablePrimArray s a -> ST s Int
stQuickSort pa = do
  let sz = PA.sizeofMutablePrimArray pa
  steps <- newSTRef 0
  go steps pa 0 (sz-1)
  readSTRef steps
  where
    go steps pa begin end = when (end>begin) $ do
      modifySTRef' steps (+1)
      let pidx = begin + ((end-begin) `div` 2)
      pidx <- partPA pa begin end pidx
      go steps pa begin (pidx-1)
      go steps pa (pidx+1) end

-- Packt quicksort nett ein

runstQuickSort :: (Show a, Ord a, Prim a) => [a] -> (Int,[a])
runstQuickSort xs = runST $ do
  let pa = PA.primArrayFromList xs
  mpa <- PA.unsafeThawPrimArray pa
  steps <- stQuickSort mpa
  -- Zum testen mal @pa@ zurueck geben! ref.trans!
  pa' <- PA.unsafeFreezePrimArray mpa
  return (steps, PA.primArrayToList pa')

-- * Vorsicht!

sq xs = runST $ sqST xs

sqST :: [Int] -> ST s [Int]
sqST xs = do
  let l = length xs
  arr <- PA.newPrimArray l
  forM_ (zip [0..] xs) $ uncurry (PA.writePrimArray arr)
  forM_ [0..l-1] $ \i ->
    PA.readPrimArray arr i >>= \x ->
      PA.writePrimArray arr i (x^2)
  forM [0..l-1] (PA.readPrimArray arr)

sicher :: [Int] -> ST s (PA.MutablePrimArray s Int)
sicher xs = do
  PA.newPrimArray (length xs)

-- leck :: [Int] -> PA.MutablePrimArray s Int
-- leck xs = runST $ sicher xs

-- | Hier sehen wir wie man "referential transparency" aushebelt.

reftrans = runST go
  where
    go :: ST s (Int,Int)
    go = do
      let k =10
      -- Array erstellen
      arr <- PA.newPrimArray k
      forM_ [1..k-1] $ \i -> PA.writePrimArray arr i (0::Int)
      -- array "fest frieren"
      cpy <- PA.freezePrimArray arr 0 k
      -- ref.trans. aushebeln
      nrt <- PA.unsafeFreezePrimArray arr
      forM_ [1..k-1] $ \i -> PA.writePrimArray arr i (9::Int)
      return (PA.indexPrimArray cpy 1, PA.indexPrimArray nrt 1)

