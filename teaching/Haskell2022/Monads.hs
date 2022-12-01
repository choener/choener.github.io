
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables #-}
{-# Language InstanceSigs #-}
{-# Language FlexibleContexts #-}
{-# Language UndecidableInstances #-}

-- recursive descent parsing
-- (error logging / monad transformers)
-- parser combinators

import Data.List (sort,subsequences,(\\))
import Data.Char (isDigit,isSpace,ord)
import Text.Printf (printf)
import Control.Monad (MonadPlus(..))
import Control.Applicative (Alternative(..))

-- | This is @Maybe@
data Option a = Nul | Has a
  deriving (Show)

-- Funktion f auf Elemente in Has anwenden
instance Functor Option where
  fmap :: (a->b) -> Option a -> Option b
  fmap f Nul     = Nul
  fmap f (Has a) = Has (f a)

-- Sowohl Funktion als auch Argument sind "eingepackt"
instance Applicative Option where
  pure :: a -> Option a
  pure = Has
  (<*>) :: Option (a->b) -> Option a -> Option b
  Nul <*> _ = Nul
  _ <*> Nul = Nul
  Has f <*> Has a = Has (f a)

-- | Entweder lhs (bevorzugt) oder rhs nutzen
instance Applicative Option => Alternative Option where
  empty :: Option a
  empty = Nul
  (<|>) :: Option a -> Option a -> Option a
  Nul <|> option = option
  Has a <|> _    = Has a

-- | a innerhalb eines "Option" Kontext bearbeiten
instance Monad Option where
  (>>=) :: Option a -> (a->Option b) -> Option b
  Nul >>= f = Nul
  Has a >>= f = f a

