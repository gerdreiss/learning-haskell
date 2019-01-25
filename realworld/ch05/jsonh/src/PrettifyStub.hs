module PrettifyStub
  ( Doc(..)
  , string
  , text
  , double
  , char
  , (<>)
  ) where

import           SimpleJSON

data Doc = ToBeDefined
       deriving (Show)

string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

char :: Char -> Doc
char c = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined
