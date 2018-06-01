-- run the program with 'starman "functionally" 5'

-- Possible Extensions
-- A real improvement to the game would be to generate a random word,
-- perhaps from a list of words or a dictionary file.
-- If you are feeling ambitious, you might try this.
-- It would involve generating a random number i and read in the ith word from a dictionary.
-- You might import System.Random and use a Haskell random number generator.


check :: String -> String -> Char -> (Bool, String)
check word display c = (c `elem` word, [if x == c then c else y | (x, y) <- zip word display])


turn :: String -> String -> Int -> IO ()
turn word display n = do
    if n == 0
        then putStrLn "You lose"
        else if word == display
             then putStrLn "You win!"
             else mkguess word display n


mkguess :: String -> String -> Int -> IO ()
mkguess word display n = do
    putStrLn (display ++ "  " ++ take n (repeat '*'))
    putStr "  Enter your guess: "
    q <- getLine
    let (correct, display') = check word display (q !! 0)
    let n' = if correct then n else n - 1
    turn word display' n'


starman :: String -> Int -> IO ()
starman word n = turn word ['-' | x <- word] n
