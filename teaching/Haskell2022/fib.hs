
{-# Language ScopedTypeVariables #-}

module Main where

import Control.Monad (when)
import Control.Parallel.Strategies
import Control.Parallel
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Environment
import Text.Printf (printf)

main :: IO ()
main = do
  as <- getArgs
  let (c,n) = case as of
        [a] -> (30, read a)
        [c,n] -> (max 2 $ read c, read n)
  when (n < 0) $ error "n < 0"
  (sR,sS) <- timewrap seqFib n
  print  sR
  putStr sS
  (pR,pS) <- timewrap (parFib c) n
  print  pR
  putStr pS
  return ()

timewrap :: (Integer -> Integer) -> Integer -> IO (Integer,String)
timewrap f k = do
  before <- getCurrentTime
  let r = f k
  after  <- seq r getCurrentTime
  return (r,printf "This took %s seconds\n" (show $ diffUTCTime after before))

seqFib :: Integer -> Integer
{-# NoInline seqFib #-}
seqFib 0 = 1
seqFib 1 = 1
seqFib n = seqFib (n-1) + seqFib (n-2)

parFib :: Integer -> Integer -> Integer
{-# NoInline parFib #-}
parFib c n | n<=2 || n<=c = seqFib n
--parFib c n = sum ([l,r] `using` parTraversable rdeepseq)
parFib c n = l `par` r `pseq` l+r
  where l = parFib c (n-1)
        r = parFib c (n-2)

