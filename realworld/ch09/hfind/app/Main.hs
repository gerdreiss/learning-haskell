module Main where

import SimpleFinder (simpleFind)
import System.FilePath (takeExtension)

main :: IO ()
main = simpleFind (\p -> takeExtension p == ".hs") "." >>= mapM_ print
