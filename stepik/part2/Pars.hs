module Pars where

import Text.Parsec

p0 :: Parsec String u (String, String)
p0 = (,) <$> many1 letter <*> many1 digit

p1 :: Parsec String u (String, String)
p1 = (,) <$> many1 letter <* many space <*> many1 digit

ignoreBraces ::
     Parsec String u a
  -> Parsec String u b
  -> Parsec String u c
  -> Parsec String u c
ignoreBraces p1 p2 p3 = p1 *> p3 <* p2

ignoreBraces1 ::
     Parsec String u a
  -> Parsec String u b
  -> Parsec String u c
  -> Parsec String u c
ignoreBraces1 = between
