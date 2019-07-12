module Bubblesort where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.STRef         (newSTRef, readSTRef, writeSTRef)

myData :: UArray Int Int
myData = listArray (0, 5) [7, 6, 4, 8, 10, 2]

myData' :: UArray Int Int
myData' = listToUArray [7, 6, 4, 8, 10, 2]

swapST :: (Int, Int) -> (Int, Int)
swapST (x, y) =
  runST $ do
    x' <- newSTRef x
    y' <- newSTRef y
    writeSTRef x' y
    writeSTRef y' x
    xfinal <- readSTRef x'
    yfinal <- readSTRef y'
    return (xfinal, yfinal)

listToUArray :: [Int] -> UArray Int Int
listToUArray vals =
  runSTUArray $ do
    let end = length vals - 1
    myArray <- newArray (0, end) 0
    forM_ [0 .. end] $ \i -> do
      let val = vals !! i
      writeArray myArray i val
    return myArray

bubblesort :: UArray Int Int -> UArray Int Int
bubblesort myArray =
  runSTUArray $ do
    stArray <- thaw myArray
    let end = (snd . bounds) myArray
    forM_ [1 .. end] $ \i ->
      forM_ [0 .. (end - i)] $ \j -> do
        val <- readArray stArray j
        nextVal <- readArray stArray (j + 1)
        let outOfOrder = val > nextVal
        when outOfOrder $ do
          writeArray stArray j nextVal
          writeArray stArray (j + 1) val
    return stArray
