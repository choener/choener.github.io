
-- | Code zu Haskell-combinatoren. Das "lens" Beispiel ist auskommentiert, da erst die "lens"
-- library installiert sein muesste.

{- Language NoImplicitPrelude -}
{-# Language TemplateHaskell #-}

module Combi where

import Prelude hiding ( (.), ($) )

-- import Control.Lens as L

(.) :: (b->c) -> (a->b) -> a -> c
(.) foo bar a = foo (bar a)
--foo . bar = \a -> foo (bar a)
($) :: (a->b) -> a -> b
foo $ a = foo a

f x = x + 1
g x = 2 * x
h x = show x ++ "!"

process = h . g . f . read

run :: IO ()
run = do
  --l <- getLine
  --putStrLn (h (g (f (read l))))
  --getLine >>= \l -> putStrLn . h . g . f . read $ l
  --putStrLn . h . g . f . read =<< getLine
  putStrLn . process =<< getLine


--data Pet = Pet { _name :: String, _age :: Int }
--  deriving (Show)
--L.makeLenses ''Pet
--
--data Person = Person
--  { _pname :: String
--  , _ppet :: Pet
--  }
--  deriving (Show)
--L.makeLenses ''Person
--
--owner = Person "Bob" (Pet "willy" 13)

