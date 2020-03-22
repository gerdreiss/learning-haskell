#!/usr/bin/env stack
-- stack runghc --resolver lts-15.4 --install-ghc

import           Data.Char
import           Data.List
import qualified Data.Map.Strict    as M
import           Data.Ord
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           System.Environment

main = do
      [fp] <- getArgs
      text <- T.readFile fp
      let histo = foldl'
                (\hash line -> M.insertWith (+) (T.length line) 1 hash)
                M.empty
                (T.lines text)
      mapM_ (\(key, value) -> print (key, value))
            (sortBy (flip (comparing snd)) (M.toList histo))

