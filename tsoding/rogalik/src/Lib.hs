module Lib where

import           Control.Monad

-- | Identity Monad interlude
newtype Foo a = Foo a

-- Functor implemented using the Monad impl
instance Functor Foo where
  fmap = liftM

-- Applicative implemented using the Monad impl
instance Applicative Foo where
  pure  = return
  (<*>) = ap

instance Monad Foo where
  Foo x >>= f = f x
-- | interlude end
