module Prettify
  ( Doc(..)
  , (<>)
  , empty
  , char
  , text
  , double
  , fsep
  , hcat
  , punctuate
  --, compact
  --, pretty
  ) where


data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show)

(<>) :: Doc -> Doc -> Doc
Empty <> y     = y
x     <> Empty = x
x     <> y     = x `Concat` y


empty :: Doc
empty = Empty


char :: Char -> Doc
char c = Char c


text :: String -> Doc
text ""  = Empty
text str = Text str


double :: Double -> Doc
double num = text (show num)


line :: Doc
line = Line


fsep :: [Doc] -> Doc
fsep = fold (</>)


(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y


softline :: Doc
softline = group line


group :: Doc -> Doc
group x = flatten x `Union` x


flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other


hcat :: [Doc] -> Doc
hcat = fold (<>)


fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty


punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds




