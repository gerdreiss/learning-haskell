module Lib where

import           Control.Monad.Cont        (when)
import           Control.Monad.Error.Class (catchError, throwError)
import           Control.Monad.Except      (ExceptT, runExceptT)
import           Control.Monad.Reader      (ReaderT, runReaderT)
import           Control.Monad.RWS.Class   (asks, get, gets, modify, put)
import           Control.Monad.State.Lazy  (State, evalState)
import           Data.Foldable             (traverse_)
import           Safe                      (readMay)

type Stack = [Integer]

--type EvalM = State Stack
--type EvalM = StateT Stack Maybe
--type EvalM = ExceptT EvalError (State Stack)
type EnvVars = [(String, Integer)]

type EvalM = ReaderT EnvVars (ExceptT EvalError (State Stack))

data EvalError
  = NotEnoughElements
  | NotANumber String
  | ExtraElements

instance Show EvalError where
  show NotEnoughElements = "Not enough elements in the expression"
  show (NotANumber s) = "Expression component '" <> s <> "' is not a number"
  show ExtraElements = "There are extra elements in the expression"

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
--
-- using the StateT monad transformer
--pop :: EvalM Integer
--pop = do
--  xs <- get
--  put (tailSafe xs)
--  lift (headMay xs)
--
-- using guard
--pop :: EvalM Integer
--pop = do
--  xs <- get
--  guard (not . null $ xs)
--  put (tail xs)
--  pure (head xs)
--
-- using exceptions
pop :: EvalM Integer
pop = do
  xs <- get
  when (null xs) $ throwError NotEnoughElements
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
--
-- using using the StateT monad transformer
--evalRPN :: String -> Maybe Integer
--evalRPN str = evalStateT evalRPN' []
--  where
-- handling exceptions
--evalRPN :: String -> Either EvalError Integer
--evalRPN str = evalState (runExceptT evalRPN') []
--  where
-- handling exceptions within the monad stack
evalRPN :: String -> EnvVars -> Either EvalError Integer
evalRPN str env = evalState (runExceptT (runReaderT evalRPN' env)) []
  where
    evalRPN' = traverse_ step (words str) >> oneElementOnStack >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    --step t   = readSafe t >>= push
    step t   = readSafe' t >>= push
    processTops op = op <$> pop <*> pop >>= push

--
--oneElementOnStack :: EvalM ()
--oneElementOnStack = do
--  l <- gets length
--  when (l /= 1) $ lift Nothing
--oneElementOnStack :: EvalM ()
--oneElementOnStack = do
--  l <- gets length
--  guard (l == 1)
--
-- using exceptions
oneElementOnStack :: EvalM ()
oneElementOnStack = do
  l <- gets length
  when (l /= 1) $ throwError ExtraElements

--
--readSafe :: String -> EvalM Integer
--readSafe s = lift (readMay s)
--
-- handling exceptions
readSafe :: String -> EvalM Integer
readSafe s = handleNaN s (readMay s)

readSafe' :: String -> EvalM Integer
readSafe' s = readSafe s `catchError` handler
  where
    handler (NotANumber s) = asks (lookup s) >>= handleNaN s
    handler e              = throwError e

handleNaN :: String -> Maybe Integer -> EvalM Integer
handleNaN s Nothing  = throwError (NotANumber s)
handleNaN _ (Just n) = pure n
