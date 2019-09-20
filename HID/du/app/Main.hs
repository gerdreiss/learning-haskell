module Main where

import           Control.Du
import           Options.Applicative

main :: IO ()
main = execParser opts >>= work
  where
    opts = info (mkConfig <**> helper) (fullDesc <> progDesc "File space usage info")
