main :: IO ()
main = do
  putStrLn "Greetings! What's your name?"
  inputStr <- getLine
  putStrLn $ "Welcome to Haskell, " ++ inputStr ++ "!"
