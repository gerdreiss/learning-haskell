{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Greetings where

import           RIO
import           RIO.Time  (getCurrentTime)
import           System.IO (hPutStrLn, stderr, stdout)

type Name = String

data App =
  App
    { appName    :: !String
    , appHandle  :: !Handle
    , appLogFunc :: !LogFunc
    }

class HasHandle env where
  handleL :: Lens' env Handle

class HasName env where
  nameL :: Lens' env Name

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasHandle Handle where
  handleL = id

instance HasHandle App where
  handleL = lens appHandle (\x y -> x {appHandle = y})

instance HasName App where
  nameL = lens appName (\x y -> x {appName = y})

switchHandle :: HasHandle env => Handle -> RIO env a -> RIO env a
switchHandle h = local (set handleL h)

addLastName :: HasName env => RIO env a -> RIO env a
addLastName = local (over nameL (++ " Smith"))

say :: HasHandle env => String -> RIO env ()
say msg = do
  h <- view handleL
  liftIO $ hPutStrLn h msg

sayTime :: HasHandle env => RIO env ()
sayTime = do
  now <- getCurrentTime
  say $ "The time is: " ++ show now

sayHello :: RIO App ()
sayHello = do
  App name _ _ <- ask
  logDebug $ fromString $ "Before saying hello to " ++ name ++ "..."
  say $ "Hello, " ++ name

sayGoodbye :: RIO App ()
sayGoodbye = do
  App name _ _ <- ask
  logDebug $ fromString $ "Before saying goodbye to " ++ name ++ "..."
  say $ "Goodbye, " ++ name
