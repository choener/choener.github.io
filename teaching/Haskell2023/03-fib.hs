
module Map where

import qualified Data.Map.Lazy as M

fib :: M.Map Integer Integer
fib = M.fromList ((0,0) : (1,1) : [(k,go k) | k <- [2..]])
  where
    go :: Integer -> Integer
    go k = fib M.! (k-1) + fib M.! (k-2)

fib' :: Integer -> Integer
fib' z = fs M.! z
  where
    fs = M.fromList [ (k,v) | k <- [0..z], let v = go k ]
    go :: Integer -> Integer
    go 0 = 0
    go 1 = 1
    go k = fs M.! (k-1) + fs M.! (k-2)

fibl :: [Integer]
fibl = 0 : 1 : [ go k | k <- [2..] ]
  where
    go k = fibl !! (k-1) + fibl !! (k-2)

fibl' :: [Integer]
fibl' = 0 : 1 : zipWith (+) fibl' (drop 1 fibl')
