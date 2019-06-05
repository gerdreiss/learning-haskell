module DescriptiveStats where

import Text.CSV
import Data.Maybe
import Data.List
import Data.Ord

noEmptyRows :: Either a CSV -> CSV
noEmptyRows ecsv = either (const []) (filter (\row -> 2 <= length row)) ecsv

{- Reads a column of data from a CSV file -}
readIndex :: Read cell => Either a CSV -> Int -> [cell]
readIndex ecsv index = map read (getIndex ecsv index)

{- Reads a column of data from a CSV file -}
getIndex :: Either a CSV -> Int -> [String]
getIndex ecsv index = map (!! index) (noEmptyRows ecsv)

range :: Ord a => [a] -> Maybe (a, a)
range []  = Nothing
range [x] = Just (x, x)
range xs  = Just (minimum xs, maximum xs)

mean :: Real a => [a] -> Maybe Double
mean []  = Nothing
mean [x] = Just $ realToFrac x
mean xs  = Just $ realToFrac (sum xs) / fromIntegral (length xs)

stdev :: Real a => [a] -> Maybe Double
stdev []  = Nothing
stdev [_] = Nothing
stdev xs  = Just $ sqrt (sumsquares / n_m1)
  where
    sumsquares   = sum $ map (diffsquare . realToFrac) xs
    diffsquare x = (x - meanxs) * (x - meanxs)
    meanxs       = fromJust (mean xs)
    n_m1         = fromIntegral (length xs - 1)

median :: Real a => [a] -> Maybe Double
median [] = Nothing
median list
  | odd (length list) = Just middleValue
  | otherwise         = Just middleEven
  where
    sorted            = sort list
    middleIndex       = length list `div` 2
    middleValue       = realToFrac $ sorted !! middleIndex
    beforeMiddleValue = realToFrac $ sorted !! (middleIndex - 1)
    middleEven        = 0.5 * (middleValue + beforeMiddleValue)

runLengthEncoding :: Ord a => [a] -> [(a, Integer)]
runLengthEncoding = map (\xs -> (head xs, genericLength xs)) . group


mode :: Ord a => [a] -> Maybe (a, Integer)
mode [] = Nothing
mode list = Just $ maximumBy (comparing snd) pairs
  where
    sorted = sort list
    pairs = runLengthEncoding sorted


countAtHour :: Int -> Int
countAtHour h = length $ filter (=~ (printf "T%02d" :: String)) timestamps