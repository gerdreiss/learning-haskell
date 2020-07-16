{-# LANGUAGE NoImplicitPrelude #-}

module Greetings where

import           RIO
import           RIO.Time  (getCurrentTime)
import           System.IO (hPutStrLn, stderr, stdout)

type Name = String

data App =
  App
    { appName   :: !String
    , appHandle :: !Handle
    }

class HasHandle env where
  handleL :: Lens' env Handle

class HasName env where
  nameL :: Lens' env Name

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

sayHello :: RIO App ()
sayHello = do
  App name _ <- ask
  say $ "Hello, " ++ name

sayTime :: HasHandle env => RIO env ()
sayTime = do
  now <- getCurrentTime
  say $ "The time is: " ++ show now

sayGoodbye :: RIO App ()
sayGoodbye = do
  App name _ <- ask
  say $ "Goodbye, " ++ name
