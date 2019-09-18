module Lib where

import           Control.Monad.State
import           Data.Foldable       (traverse_)
import           Safe

type Stack = [Integer]

--type EvalM = State Stack
type EvalM = StateT Stack Maybe

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
--pop :: EvalM Integer
--pop = state $ \(x:xs) -> (x, xs)
-- using the StateT monad transformer
--pop :: EvalM Integer
--pop = do
--  xs <- get
--  put (tailSafe xs)
--  lift (headMay xs)
-- using guard
pop :: EvalM Integer
pop = do
  xs <- get
  guard (not . null $ xs)
  put (tail xs)
  pure (head xs)

-- naive implementation
--evalRPN :: String -> Integer
--evalRPN expr = evalState evalRPN' []
--  where
--    evalRPN' = traverse_ step (words expr) >> pop
--    step "+" = processTops (+)
--    step "*" = processTops (*)
--    step t   = push (read t)
--    processTops op = flip op <$> pop <*> pop >>= push
-- using using the StateT monad transformer
evalRPN :: String -> Maybe Integer
evalRPN str = evalStateT evalRPN' []
  where
    evalRPN' = traverse_ step (words str) >> oneElementOnStack >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step t   = readSafe t >>= push
    processTops op = op <$> pop <*> pop >>= push

--oneElementOnStack :: EvalM ()
--oneElementOnStack = do
--  l <- gets length
--  when (l /= 1) $ lift Nothing
oneElementOnStack :: EvalM ()
oneElementOnStack = do
  l <- gets length
  guard (l == 1)

readSafe :: String -> EvalM Integer
readSafe s = lift (readMay s)
