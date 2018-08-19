
import           Control.Monad

type Position = (Int, Int)

main :: IO ()
main = print $ (6, 2) `canReachIn3` (6, 1)

moveKnight :: Position -> [Position]
moveKnight (col, row) = do
    (col', row') <- [ (col + 2, row - 1)
                    , (col + 2, row + 1)
                    , (col - 2, row - 1)
                    , (col - 2, row + 1)
                    , (col + 1, row - 2)
                    , (col + 1, row + 2)
                    , (col - 1, row - 2)
                    , (col - 1, row + 2) ]
    guard (col' `elem` [1..8] && row' `elem` [1..8])
    return (col', row')

in3 :: Position -> [Position]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
    -- do
    -- first <- moveKnight start
    -- second <- moveKnight first
    -- moveKnight second

canReachIn3 :: Position -> Position -> Bool
canReachIn3 start end = end `elem` in3 start

-- TODO
reachIn3 :: Position -> Position -> [Position]
reachIn3 start end = []
