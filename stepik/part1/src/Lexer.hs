module Lexer
  ( Token(..)
  , asToken
  ) where

import Data.Char


data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken s
  | all isDigit s = Just $ Number $ read s
  | s == "+"  = Just Plus
  | s == "-"  = Just Minus
  | s == "("  = Just LeftBrace
  | s == ")"  = Just RightBrace
  | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize = mapM asToken . words
  -- sequence . map asToken . words
