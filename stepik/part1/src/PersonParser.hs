module PersonParser
  ( Error(..)
  , Person(..)
  , parsePerson
  , parseP
  , tupP
  , linesP
  , stripP
  ) where

import           Data.Char (isDigit)
import           Data.List (groupBy, transpose)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving (Show,Eq)

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Show,Eq)

parsePerson :: String -> Either Error Person
parsePerson str = case parseP str of
    fst : lst : age : _ -> if all isDigit age then Right Person { firstName = fst, lastName = lst, age = read age :: Int } else Left $ IncorrectDataError age
    otherwise -> Left IncompleteDataError

parseP :: String -> [String]
parseP str = map (dropWhile (==' ') . reverse . takeWhile (/='=') . reverse) $  map (filter (/='\n')) $ groupBy (\_ b -> b /= '\n') str

tupP :: String -> (String, String)
tupP str = (stripP $ takeWhile (/='=') str, stripP $ tail $ dropWhile(/='=') str)

linesP :: String -> [String]
linesP str = if '\n' `elem` str then map (filter (/='\n')) $ groupBy (\_ b -> b /= '\n') str else [str]

stripP :: String -> String
stripP = reverse . dropWhile (==' ') . reverse . dropWhile (==' ')



-- Tests
-- wrong Parse | empty string
t0 = parsePerson "" == Left IncompleteDataError
-- correct
t1 = parsePerson "firstName = John\nlastName = Connor\nage = 30" == Right (Person {firstName = "John", lastName = "Connor", age = 30})
-- correct | shiffled fields
t18 = parsePerson "lastName = Connor\nfirstName = John\nage = 30"
-- wrong Parse | no spaces around = in minor fields
t2 = parsePerson "firstName = John Smith\nlastName = Connor\nage = 30\nasde=as11"
 -- wrong Parse | no spaces around = on the left in minor fields
t5 = parsePerson "firstName = John Smith\nlastName = Connor\nage = 30\nasde= "
-- wrong Parse | no spaces around = in major fields
t3 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30"
 -- wrong Incorrect | age is non-numeric
t4 = parsePerson "firstName = John\nlastName = Connor\nage = as30"
-- wrong Parse | no spaces around = in major fields, missing major field
t6 = parsePerson "firstName=Barbarian\nlastName=Conn Or"
-- wrong Parse | no spaces around = in major fields, typo in major field
t7 = parsePerson "firstNameee = John Smith\nlastName = Connor\nage = 30\nasde=as11"
-- correct | excessive fields
t8 = parsePerson "firstName = John\nlastName = Connor\nfoo = bar\nage = 30"
-- wrong Incomplete | missing major field
t9 = parsePerson "firstName = Barbarian\nlastName = Conn Or"
-- wrong Parse | empty major value
t10 = parsePerson "firstName = John\nlastName = Connor\nage = "
-- wrong Parse | no spaces around = on the right in major field
t11 = parsePerson "firstName = John\nlastName = Connor\nage ="
-- wrong Parse | empty key, missing major field
t12 = parsePerson "firstName = John\nlastName = Connor\n = 30"
-- correct | spaces in major field value
t13 = parsePerson "firstName = Barbarian\nlastName = Conn On\nage = 30"
-- correct | = in major field value
t14 = parsePerson "firstName = John\nlastName = Con=nor\nage = 30"
-- wrong Parse | no spaces around =, missing value in minor field
t15 = parsePerson "firstName=Barbarian\nlastName=Conn On\nage=30\ng dsfsd"
-- wrong Incomplete | major field key with whitespace, age is non-numeric
t17 = parsePerson " firstName = John\nlastName = Connor\nage = 2f8 "
