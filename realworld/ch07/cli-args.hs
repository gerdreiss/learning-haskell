import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  print (prog : args)
