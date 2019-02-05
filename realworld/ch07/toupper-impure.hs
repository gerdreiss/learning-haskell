import Data.Char (toUpper)
import System.IO

main :: IO ()
main = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode  
  hPutStrLn outh "TOUPPERCASED TEXT FROM input.txt:\n\n\""
  mainloop inh outh
  hPutStrLn outh "\""
  hClose inh
  hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = do
  ineof <- hIsEOF inh
  if ineof
    then return ()
    else do
      inputStr <- hGetLine inh
      hPutStrLn outh (map toUpper inputStr)
      mainloop inh outh
