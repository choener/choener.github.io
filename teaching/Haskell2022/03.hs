
-- The smallest free number
-- @ :set +s @

import Data.Array (Array)
import Prelude hiding (notElem, takeWhile)
import qualified Control.Monad.ST as ST
import qualified Data.Array as A
import qualified Data.Array.ST as A

--{{{ (\\)
us \\ vs = filter (notElem vs) us

notElem [] r = True
notElem (l:ls) r = l /= r && notElem ls r
--}}}

--{{{ takeWhile
takeWhile f [] = []
takeWhile f (x:xs) = if f x then x : takeWhile f xs else []
--}}}

--{{{ Zuerst: verstaendliche Lsg
minFrei xs = head ([0..] \\ xs)
--}}}


-- * Array Lsg

--{{{ search
search :: Array Int Bool -> Int
search = length . takeWhile id . A.elems
--}}}

--{{{ checklist
checklist :: [Int] -> Array Int Bool
checklist xs = A.accumArray (||) False (0,n) $ zip (filter (<=n) xs) (repeat True)
  where n = length xs
        (∨) = (||)    -- looks too much like @vee@ (the letter)
--}}}


--{{{ checklistST
checklistST :: [Int] -> Array Int Bool
checklistST xs = A.runSTArray $ do
  let n = length xs
  a <- A.newArray (0,n) False
  sequence_ [ A.writeArray a x True
            | x <- xs, x <= n ]
  return a
--}}}

--{{{ minFreiA
minFreiA = search . checklist
--}}}

-- * Divide and Conquer

--{{{ minVon
minVon :: Int -> [Int] -> Int
minVon a xs = head ([a..] \\ xs)
--}}}

--{{{ partition
partition :: (a -> Bool) -> [a] -> ([a],[a])
partition p xs = foldr (select p) ([],[]) xs
--}}}

-- {{{ select
select :: (a -> Bool) -> a -> ([a],[a]) -> ([a],[a])
select p x ~(ts,fs)
  | p x = (x:ts, fs)
  | otherwise = (ts,x:fs)
--}}}

--{{{ minFreiDC
minFreiDC = minVonDC 0
--}}}

--{{{ minVonDC
minVonDC :: Int -> [Int] -> Int
minVonDC a xs
  | null xs = a
  | length us == b-a = minVonDC b vs
  | otherwise = minVonDC a us
  where
    (us,vs) = partition (<b) xs
    b = a + 1 + n `div` 2
    n = length xs
--}}}

-- * D & C performance

--{{{ minFreiDCP, minVonDCP
minFreiDCP xs = minVonDCP 0 (length xs, xs)

minVonDCP a (n,xs)
  | n == 0 = a
  | m == b-a = minVonDCP b (n-m, vs)
  | otherwise = minVonDCP a (m,us)
  where
    (us,vs) = partition (<b) xs
    b = a + 1 + n `div` 2
    m = length us
--}}}

