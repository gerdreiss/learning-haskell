{-# LANGUAGE NoImplicitPrelude #-}

import           Greetings
import           RIO
import           RIO.Time  (getCurrentTime)
import           System.IO (hPutStrLn, stderr, stdout)

main :: IO ()
main = do
  runRIO App {appName = "Ivan", appHandle = stderr} $ do
    sayHello
    sayTime
    sayGoodbye
  runRIO stdout sayTime
