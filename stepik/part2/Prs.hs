module Prs where

import           Control.Applicative hiding (many)
import           Data.Char

newtype Prs a =
  Prs
    { runPrs :: String -> Maybe (a, String)
    }

instance Functor Prs where
  fmap f p = Prs fun
    where
      fun s =
        case runPrs p s of
          Nothing      -> Nothing
          Just (a, s') -> Just (f a, s')

--fmap :: (a -> b) -> Prs a -> Prs b
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
  pure a = Prs func
    where
      func s = return (a, s)
  pf <*> pv = Prs func
    where
      func s = do
        (g, s') <- runPrs pf s
        (a, s'') <- runPrs pv s'
        return (g a, s'')

instance Alternative Prs where
  empty = Prs $ const Nothing
  p <|> q = Prs func
    where
      func s =
        let ps = runPrs p s
         in if null ps
              then runPrs q s
              else ps

anyChr :: Prs Char
anyChr = Prs f
  where
    f []     = Nothing
    f (c:cs) = Just (c, cs)

many :: Prs a -> Prs [a]
many p = (:) <$> p <*> many p <|> pure []

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> many p
