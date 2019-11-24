module Json0 where

import           Control.Applicative
import           Data.Bool
import           Data.Char

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- NOTE: no support for floats
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

--parser :: String -> Either (Int, Int, String) (String, a)
newtype Parser a =
  Parser
    { runParser :: String -> Maybe (String, a) -- NOTE: no proper error reporting
    }

instance Functor Parser where
  fmap f (Parser parse) =
    Parser $ \input -> do
      (rest, result) <- parse input
      Just (rest, f result)

{- Applicative impl for Parser. Note that
   'fp' of the first Parser is a partially applied function,
   because a call to <*> comes usually after
   calling <$> on the first Parser that returns a Parser
   with a partially applied function fp. e.g.
   f <$> Parser a <*> Parser b
   so this call returns the rest of the input
   and the partially applied function
-}
instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser fp) <*> (Parser p) =
    Parser $ \input -> do
      (input', f) <- fp input
      (input'', a) <- p input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
-- stringP  = sequenceA . map charP
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

jsonNull :: Parser JsonValue
-- jsonNull = (\_ -> JsonNull) <$> stringP "null"
-- jsonNull = (const JsonNull) <$> stringP "null"
jsonNull = JsonNull <$ stringP "null"

-- should take strictly true or false, no trueee nor falsee
jsonBool :: Parser JsonValue
jsonBool = mkJsonBool <$> (stringP "true" <|> stringP "false")
  where
    mkJsonBool s = JsonBool (s == "true")
    --mkJsonBool "true"  = JsonBool True
    --mkJsonBool "false" = JsonBool False
    --mkJsonBool _       = undefined -- should never happen

jsonNumber :: Parser JsonValue
jsonNumber = toJsonNumber <$> notNull (spanP isDigit)
  where
    toJsonNumber ds = JsonNumber $ read ds

-- NOTE: no escape support
stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> ws *> pairs <* ws <* charP '}')
  where
    pairs = sepBy (ws *> charP ',' <* ws) pair
    pair = (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> charP ':' *> ws) <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject
