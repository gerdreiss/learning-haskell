{-# LANGUAGE NoImplicitPrelude #-}

import           Greetings
import           RIO
import           RIO.Time  (getCurrentTime)
import           System.IO (hPutStrLn, stderr, stdout)

main :: IO ()
main = do
  logOptions' <- logOptionsHandle stderr False
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  withLogFunc logOptions $ \logFunc -> do
    runRIO (app logFunc) $ do
      sayHello
      sayTime
      sayGoodbye
    runRIO stdout sayTime

app :: LogFunc -> App
app logFunc =
  App { appName = "Ivan", appHandle = stderr, appLogFunc = logFunc }
