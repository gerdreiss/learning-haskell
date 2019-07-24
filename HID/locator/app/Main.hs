{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Algebra
import Locator
import Data.List

deriving instance BoundedEnum Direction
deriving instance Ord Turn

test :: Bool
test = sort (nub [ findTurn d1 d2 | d1 <- range, d2 <- range ]) == range

main :: IO ()
main = print test
