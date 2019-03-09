module SimpleFinder (simpleFind) where

import RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = filter p <$> getRecursiveContents path
