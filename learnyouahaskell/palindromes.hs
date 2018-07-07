respondPalindromes :: String -> String
respondPalindromes = unlines . map (\xs -> if isPal xs then "palindrome" else "not a palindrome") . lines

isPal :: String -> Bool
isPal [] = False
isPal xs = xs == reverse xs

main = interact respondPalindromes

