module Monands where

    class Monad m where
        (>>=) :: m a -> (a -> m b) -> m b
        return :: a -> m a

    class Functor m => MonadAlt m where
        join :: m(m a) -> m a
        returnAlt :: a -> m a
