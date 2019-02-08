main :: IO ()
main =
  putStrLn "Greetings! What's your name?" >>
  getLine >>=
  (\inputStr -> putStrLn $ "Welcome to Haskell, " ++ inputStr ++ "!")
