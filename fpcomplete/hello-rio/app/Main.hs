{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           RIO
import           RIO.Time  (getCurrentTime)
import           System.IO (hPutStrLn, stderr, stdout)

data App = App
    { appName   :: !String
    , appHandle :: !Handle
    }

class HasHandle env where
  getHandle :: env -> Handle

instance HasHandle Handle where
  getHandle = id

instance HasHandle App where
  getHandle = appHandle

main :: IO ()
main = do
  let app = App { appName = "Alice", appHandle = stderr }
  runRIO app $ do
    sayHello
    sayTime
    sayGoodbye
  -- Also works!
  runRIO stdout sayTime

say :: HasHandle env => String -> RIO env ()
say msg = do
  env <- ask
  liftIO $ hPutStrLn (getHandle env) msg

sayTime :: HasHandle env => RIO env ()
sayTime = do
  now <- getCurrentTime
  say $ "The time is: " ++ show now

sayHello :: RIO App ()
sayHello = do
  App name _h <- ask
  say $ "Hello, " ++ name

sayGoodbye :: RIO App ()
sayGoodbye = do
  App name _h <- ask
  say $ "Goodbye, " ++ name
