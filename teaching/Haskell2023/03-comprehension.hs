
module Comprehensions where

-- [(1,2,4),(1,3,9),(3,2,36),(3,3,81)]
compr :: [(Int,Int,Int)]
compr = [ (x,y,x^2*y^2) | x <- [1..3], x `mod` 2 == 1
                          -- Generator,  Praedikat
        , y <- [2..3] ]

factors :: Integer -> [Integer]
factors p = [ f | f <- [1..p], p `mod` f == 0 ]

isPrime :: Integer -> Bool
isPrime p = [1,p] == factors p

primes :: [Integer]
primes = [ p | p <- [2..], isPrime p ]

primes2 :: [Integer]
primes2 = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p
             : sieve [ x
                     | x <- xs
                     , x `mod` p > 0 ]

