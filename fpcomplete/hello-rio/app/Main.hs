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
  let app = App {appName = "Alice", appHandle = stderr}
  runRIO app $ do
    switchHandle stdout sayHello
    sayTime

switchHandle :: Handle -> RIO App a -> RIO App a
switchHandle h inner = do
  app <- ask
  let app' = app {appHandle = h}
  runRIO app' inner

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
