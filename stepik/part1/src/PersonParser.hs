module PersonParser
  ( Error(..)
  , Person(..)
  , parsePerson
  ) where

import           Data.Char  (isDigit)
import           Data.List  (groupBy, transpose)
import           Data.Maybe (fromJust)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson str
  | stripP str == "" = Left ParsingError
  | head str == ' ' = Left ParsingError
  | last str == ' ' = Left ParsingError
  | not $ substring "firstName = " str = Left ParsingError
  | not $ substring "\nlastName = " str = Left ParsingError
  | not $ substring "\nage = " str = Left ParsingError
  | otherwise =
    case parseP str of
      (Just fn, Just ln, Just a) ->
        if all isDigit a
          then Right Person {firstName = fn, lastName = ln, age = read a :: Int}
          else Left $ IncorrectDataError a
      (Nothing, Nothing, Nothing) -> Left ParsingError
      _ -> Left IncompleteDataError

parseP :: String -> (Maybe String, Maybe String, Maybe String)
parseP "" = (Nothing, Nothing, Nothing)
parseP str =
  let tups = map tupP . lines $ str
   in ("firstName" `lookup` tups, "lastName" `lookup` tups, "age" `lookup` tups)

tupP :: String -> (String, String)
tupP str = (stripP $ takeWhile (/= '=') str, stripP $ tail $ dropWhile (/= '=') str)

stripP :: String -> String
stripP = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs ys
  | prefix xs ys = True
  | substring xs (tail ys) = True
  | otherwise = False

prefix :: String -> String -> Bool
prefix [] ys         = True
prefix (x:xs) []     = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys
