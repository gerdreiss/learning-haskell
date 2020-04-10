module Parser where

import           Control.Applicative hiding (many)
import           Data.Char

newtype Parser a =
  Parser
    { apply :: String -> [(a, String)]
    }

instance Functor Parser where
  fmap f p = Parser fun
    where
      fun s = [(f a, s') | (a, s') <- apply p s]

instance Applicative Parser where
  pure a = Parser func
    where
      func s = [(a, s)]
  pf <*> pv = Parser func
    where
      func s = do
        (g, s') <- apply pf s
        (a, s'') <- apply pv s'
        return (g a, s'')

instance Alternative Parser where
  empty = Parser $ const []
  p <|> q = Parser func
    where
      func s =
        let ps = apply p s
         in if null ps
              then apply q s
              else ps

parse :: Parser a -> String -> a
parse p = fst . head . apply p

anyChar :: Parser Char
anyChar = Parser f
  where
    f ""     = []
    f (c:cs) = [(c, cs)]

lower :: Parser Char
lower = Parser f
  where
    f "" = []
    f (c:cs)
      | c `elem` "abcdefghijklmnoprstuvwxyz" = [(c, cs)]
      | otherwise = []

lowers :: Parser String
lowers = (:) <$> lower <*> lowers <|> pure []

many :: Parser a -> Parser [a]
many p = (:) <$> p <*> many p <|> pure []
