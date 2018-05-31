module Functions (og, coordsComprehension) where


data Cell = Cell (Integer, Integer) Char deriving (Eq, Ord, Show)


og :: Show a => [a] -> IO ()
og = putStrLn . unlines . map show

coordsComprehension :: [[Int]]
coordsComprehension = [ [ (row, col) | col <- [0..7] ] | row <- [0..7] ]

div2 x = x `mod` 2 == 0

doubled = do
    i <- [0..]
    return (i * 2)

filtered = do
    i <- [0..]
    guard (div2 i)

doubledAndFiltered = do
    i <- [0..]
    guard (div2 i)
    return (i * 2)

doubledAndFilteredComprehension = [ i * 2 | i <- [0..], div2 i ]

