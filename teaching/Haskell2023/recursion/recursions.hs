{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}

module OhNine where

import Prelude hiding (foldr, foldl, foldl')


-- * left and right folds

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b [] = b
foldr f b (x:xs) = f x (foldr f b xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ b [] = b
foldl f b (x:xs) = foldl f (f b x) xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ b [] = b
foldl' f b (x:xs) = let z = f b x
  in z `seq` foldl' f z xs

unfoldr :: (b -> Maybe (a,b)) -> b -> [a]
unfoldr f b = case f b of
  Nothing -> []
  Just (a,b') -> a : unfoldr f b'

-- * recursion schemes

--

class Functor (Base t) => Recursive t where
  project :: t -> Base t t

--

data Tree a = Node a [Tree a]
  deriving (Show)

type ForestF a b = [b]

--

data TreeF a b = NodeF a (ForestF a b)

instance Functor (TreeF a) where
  fmap f (NodeF x xs) = NodeF x (fmap f xs)

type instance Base (Tree a) = TreeF a

instance Recursive [a] where
  project [] = NilF
  project (x:xs) = ConsF x xs

instance Recursive (Tree a) where
  project (Node x xs) = NodeF x xs

type family Base t :: * -> *

--

data ListF a b = NilF | ConsF a b

instance Functor (ListF a) where
  fmap f NilF = NilF
  fmap f (ConsF a b) = ConsF a (f b)

type instance Base [a] = ListF a

--

cata :: Recursive t => (Base t a -> a) -> t -> a
cata f = c where c = f . fmap c . project

testcata = cata (\x -> case x of { NilF -> 0; ConsF a b -> a+b}) [1..4]

para :: Recursive t => (Base t (t,a) -> a) -> t -> a
para t = p where p x = t . fmap ((,) <*> p) $ project x

testpara1 = para (\x -> case x of { NilF -> 0; ConsF a (bs,b) -> a+b}) [1..4]
testpara2 = para (\x -> case x of { NilF -> [[]]; ConsF a (rs,as) -> [rs,[a]] : as}) [1..4]


t = Node 1 [Node 2 [], Node 3 []]

g (NodeF x xs) = x + sum xs


-- * Fix (again!)

--newtype Fix f = In { unFix :: f (Fix f) }
--
--data ListF a b = NilF | ConsF a b
--type List a = Fix (ListF a)
--
--cata :: Functor f => (f b -> b) -> Fix f -> b
--cata alg (In f) = alg (fmap (cata alg) f)

