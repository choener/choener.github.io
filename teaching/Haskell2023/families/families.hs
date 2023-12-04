{-# LANGUAGE TypeFamilies #-}

-- | Demomodul zu Typfamilien

module Families where



type family List a :: *

type instance List () = Int

type instance List Int = [Int]

class ListFuns a where
  lfcons :: a -> List a -> List a
  lfuncons :: List a -> (a, List a)
  lfnull :: a -> List a

instance ListFuns () where
  lfcons _ k = k+1
  lfuncons k | k > 0 = ((), k-1)
  lfnull _ = 0

instance ListFuns Int where
  lfcons a as = a : as
  lfuncons (a:as) = (a,as)
  lfnull _ = []



class TFList a where
  type TFL a :: *
  tfcons :: a -> TFL a -> TFL a
  tfuncons :: TFL a -> (a, TFL a)
  tfnull :: a -> TFL a

instance TFList Int where
  type TFL Int = [Int]
  tfcons a as = a:as
  tfuncons (a:as) = (a,as)
  tfnull _ = []



class DFList a where
  data DFL a :: *
  dfcons :: a -> DFL a -> DFL a
  dfuncons :: DFL a -> (a, DFL a)
  dfnull :: DFL a

instance DFList Int where
  newtype DFL Int = DFInt [Int]
  dfcons a (DFInt as) = DFInt (a:as)
  dfuncons (DFInt (a:as)) = (a, DFInt as)
  dfnull = DFInt []

deriving instance Show (DFL Int)
