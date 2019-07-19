module Lib where

import           Control.Parallel.Strategies (Eval, rpar, runEval)

type Strategy a = a -> Eval a

--parPair :: Strategy (a, b)
--parPair (a, b) = do
--  a' <- rpar a
--  b' <- rpar b
--  return (a', b')

--using :: a -> Strategy a -> a
--x `using` s = runEval (s x)

-- example;
-- (fib 24, fib 25) `using` parPair
evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa sb (a, b) = do
  a' <- sa a
  b' <- sb b
  return (a', b')

parPair :: Strategy (a,b)
parPair = evalPair rpar rpar

