module Results where

data Result a
  = Ok a
  | Error String
  deriving (Eq, Show)

instance Functor Result where
  fmap _ (Error s) = Error s
  fmap f (Ok a)    = Ok (f a)

instance Applicative Result where
  pure = Ok
  Error s <*> _ = Error s
  _ <*> Error s = Error s
  Ok f <*> Ok a = Ok (f a)

instance Foldable Result where
  foldr f ini (Ok x)    = f x ini
  foldr _ ini (Error _) = ini

instance Traversable Result where
  traverse _ (Error s) = pure (Error s)
  traverse f (Ok a)    = Ok <$> f a
