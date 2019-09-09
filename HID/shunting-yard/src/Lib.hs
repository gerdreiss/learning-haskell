module Lib where

import           Control.Monad.State.Lazy
import           Data.Char                (isDigit, isSpace)
import           Data.Foldable            (traverse_)
import           Data.List                (groupBy)

data Expr a
  = Lit a
  | Add (Expr a) (Expr a)
  | Mult (Expr a) (Expr a)

type Token = String
type Stack = [Token]
type Output = [Expr Integer]
type MyState = (Stack, Output)

--
isEmpty :: State MyState Bool
isEmpty = null <$> gets fst

--
notEmpty :: State MyState Bool
notEmpty = not <$> isEmpty

--
top :: State MyState Token
top = gets (head . fst) -- let it crash on empty stack

--
pop :: State MyState Token
pop = do
  (t:s, es) <- get -- let it crash on empty stack
  put (s, es)
  pure t

--
pop_ :: State MyState () -- let it crash on empty stack
pop_ = modify (\(s, es) -> (tail s, es))

--
push :: Token -> State MyState ()
push t = modify (\(s, es) -> (t : s, es))

whileNotEmptyAnd :: (Token -> Bool) -> State MyState () -> State MyState ()
whileNotEmptyAnd pred m = go
  where
    go = do
      b1 <- notEmpty
      when b1 $ do
        b2 <- pred <$> top
        when b2 (m >> go)

output :: Token -> State MyState ()
output t = modify (builder t <$>)
  where
    builder "+" (e1:e2:es) = Add e1 e2 : es
    builder "*" (e1:e2:es) = Mult e1 e2 : es
    builder n es           = Lit (read n) : es -- let it crash on not a number

isOp "+" = True
isOp "*" = True
isOp _   = False

precedence "+" = 1
precedence "*" = 2
precedence _   = 0

t1 `precGTE` t2 = precedence t1 >= precedence t2

convertToExpr :: String -> Expr Integer
convertToExpr str = head $ snd $ execState convert ([], [])
  where
    convert = traverse_ processToken (reverse $ tokenize str) >> transferRest
    processToken ")" = push ")"
    processToken "(" = transferWhile (/= ")") >> pop_
    processToken t
      | isOp t = transferWhile (`precGTE` t) >> push t
      | otherwise = output t -- number
    transfer = pop >>= output
    transferWhile pred = whileNotEmptyAnd pred transfer
    transferRest = transferWhile (const True)
    tokenize = groupBy (\a b -> isDigit a && isDigit b) . filter (not . isSpace)
