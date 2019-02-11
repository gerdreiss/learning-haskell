main :: IO ()
main = do
  putStrLn "Greeting once again. What's your name?"
  inputStr <- getLine
  let outStr = name2reply inputStr
  putStrLn outStr

name2reply :: String -> String
name2reply name =
  "Pleased to meet you, " ++ name ++ ".\n" ++
  "Your name contains " ++ cc ++ " characters."
  where cc = show $ length name
