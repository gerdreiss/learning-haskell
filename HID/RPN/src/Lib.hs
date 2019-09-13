module Lib where

import           Control.Monad.State
import           Data.Foldable       (traverse_)

type Stack = [Integer]

type EvalM = State Stack

--
push :: Integer -> EvalM ()
push x = modify (x :)

-- long version:
--pop :: EvalM Integer
--pop = do
--  xs <- get
--  put (tail xs)
--  pure (head xs)
-- shorter version:
pop :: EvalM Integer
pop = state $ \(x:xs) -> (x, xs)

--
evalRPN :: String -> Integer
evalRPN expr = evalState evalRPN' []
  where
    evalRPN' = traverse_ step (words expr) >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step t   = push (read t)
    processTops op = flip op <$> pop <*> pop >>= push
