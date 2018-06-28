module Loggers
  ( Log(..)
  , toLogger
  , execLoggers
  , execLoggersList
  ) where

data Log a = Log [String] a

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = Log [msg] . f

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g =
  let Log [msg1] y = f x
      Log [msg2] z = g y
   in Log [msg1, msg2] z

-- Kleisli arrow
returnLog :: a -> Log a
returnLog = Log []

-- bind
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log xmsgs x) f = Log (xmsgs ++ ymsgs) y
  where
    (Log ymsgs y) = f x


execLoggersList :: a -> [a -> Log a] -> Log a
-- execLoggersList x []     = returnLog x
-- execLoggersList x (f:fs) = f x `bindLog` (flip execLoggersList $ fs)

-- Wow!
execLoggersList = foldl bindLog . returnLog