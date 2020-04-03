module Pars where

import Text.Parsec

p0 :: Parsec [Char] u ([Char], [Char])
p0 = (,) <$> many1 letter <*> many1 digit

p1 :: Parsec [Char] u ([Char], [Char])
p1 = (,) <$> many1 letter <* many space <*> many1 digit

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces p1 p2 p3 = p1 *> p3 <* p2

ignoreBraces1 :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces1 = between
