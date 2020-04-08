module Prs where

import           Control.Applicative hiding (many)
import           Data.Char

newtype Prs a = Prs
 { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
--fmap :: (a -> b) -> Prs a -> Prs b
  fmap f p = Prs fun where
    fun s = case runPrs p s of
      Nothing      -> Nothing
      Just (a, s') -> Just (f a, s')

-- this is mine
-- instance Applicative Prs where
--   pure a = Prs fun where
--     fun s = Just (a, s)
--   pf <*> pv = Prs fun where
--     fun s = case runPrs pf s of
--       Nothing -> Nothing
--       Just (f, s') -> case runPrs pv s' of
--         Nothing       -> Nothing
--         Just (a, s'') -> Just (f a, s'')

-- but this is much nicer
instance Applicative Prs where
  pure a = Prs func where
    func s = return (a, s)
  pf <*> pv = Prs func where
    func s = do
      (g, s')  <- runPrs pf s
      (a, s'') <- runPrs pv s'
      return (g a, s'')

anyChr :: Prs Char
anyChr = Prs f where
  f []     = Nothing
  f (c:cs) = Just (c, cs)
