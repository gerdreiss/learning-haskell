import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    print . solveRPN . head $ args


solveRPN :: String -> Double
solveRPN = head . foldl foldF [] . words
    where foldF (x:y:ys) "*" = (y * x):ys
          foldF (x:y:ys) "+" = (y + x):ys
          foldF (x:y:ys) "-" = (y - x):ys
          foldF (x:y:ys) "/" = (y / x):ys
          foldF xs s         = read s:xs
