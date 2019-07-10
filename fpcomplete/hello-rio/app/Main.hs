{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO
import RIO.Time (getCurrentTime)
import System.IO (hPutStrLn, stderr)

data App = App
  { appName :: !String
  , appHandle :: !Handle
  }

main :: IO ()
main = do
  let app = App
        { appName = "Alice"
        , appHandle = stderr
        }
  runRIO app $ do
    sayHello
    sayTime
    sayGoodbye

say :: String -> RIO App ()
say msg = do
  App _name h <- ask
  liftIO $ hPutStrLn h msg

sayHello :: RIO App ()
sayHello = do
  App name _h <- ask
  say $ "Hello, " ++ name

sayTime :: RIO App ()
sayTime = do
  now <- getCurrentTime
  say $ "The time is: " ++ show now

sayGoodbye :: RIO App ()
sayGoodbye = do
  App name _h <- ask
  say $ "Goodbye, " ++ name

