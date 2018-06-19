module PersonParser
  ( Error(..)
  , Person(..)
  , parsePerson
  ) where

import           Data.Char  (isDigit)
import           Data.List  (groupBy, transpose)
import           Data.Maybe (fromJust)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving (Show, Eq)

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Show, Eq)

parsePerson :: String -> Either Error Person
parsePerson "" = Left IncompleteDataError
parsePerson str =
  case parseP str of
    (Just fn, Just ln, Just a) ->
      if all isDigit a
        then Right Person {firstName = fn, lastName = ln, age = read a :: Int}
        else Left $ IncorrectDataError a
    _ -> Left IncompleteDataError

parseP :: String -> (Maybe String, Maybe String, Maybe String)
parseP "" = (Nothing, Nothing, Nothing)
parseP str =
  let tups = map tupP . lines $ str
   in ("firstName" `lookup` tups, "lastName" `lookup` tups, "age" `lookup` tups)

linesP :: String -> [String]
linesP "" = []
linesP str =
  if '\n' `elem` str
    then map (filter (/= '\n')) $ groupBy (\_ b -> b /= '\n') str
    else [str]

tupP :: String -> (String, String)
tupP str = (stripP $ takeWhile (/= '=') str, stripP $ tail $ dropWhile (/= '=') str)

stripP :: String -> String
stripP = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
