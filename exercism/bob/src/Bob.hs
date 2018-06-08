{-# LANGUAGE TemplateHaskell #-}

module Bob (responseFor) where

import           Data.Char

-- Bob is a lackadaisical teenager. In conversation, his responses are very limited.
-- Bob answers 'Sure.' if you ask him a question.
-- He answers 'Whoa, chill out!' if you yell at him.
-- He answers 'Calm down, I know what I'm doing!' if you yell a question at him.
-- He says 'Fine. Be that way!' if you address him without actually saying anything.
-- He answers 'Whatever.' to anything else.

responseFor :: String -> String
responseFor xs
  | yell xs && question xs = "Calm down, I know what I'm doing!"
  | yell xs = "Whoa, chill out!"
  | question xs = "Sure."
  | sayNothing xs = "Fine. Be that way!"
  | otherwise = "Whatever."


yell :: String -> Bool
yell [] = False
yell xs = any isLetter xs && (null $ filter isLower xs)

question :: String -> Bool
question [] = False
question xs = length stripped > 0 && last stripped == '?'
  where stripped = strip xs

strip :: String -> String
strip []     = []
strip (x:xs) = if isSpace x then strip xs else x:(strip xs)

sayNothing :: String -> Bool
sayNothing [] = True
sayNothing xs = null $ filter isAlphaNum xs
