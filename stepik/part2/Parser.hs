module Parser where

import Control.Applicative hiding (many)
import Data.Char

newtype Parser a =
  Parser
    { apply :: String -> [(a, String)]
    }

instance Functor Parser where
  fmap f p = Parser fun
    where
      fun s = [(f a, s') | (a, s') <- apply p s]

parse :: Parser a -> String -> a
parse p = fst . head . apply p

anyChar :: Parser Char
anyChar = Parser f
  where
    f "" = []
    f (c:cs) = [(c, cs)]
