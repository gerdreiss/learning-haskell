{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad        (when)
import qualified Data.ByteString.Lazy as BL (readFile)
import           Data.Csv             (decodeByName)
import qualified Data.Text.IO         as TIO

import           Charts
import           Params
import           QuoteData
import           Statistics
import           StatReport

main :: IO ()
main = cmdLineParser >>= work

work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err          -> putStrLn err
    Right (_, quotes) -> generateReports params quotes

generateReports :: (Functor t, Foldable t) => Params -> t QuoteData -> IO ()
generateReports Params {..} quotes = do
  TIO.putStr $ statReport statInfo'
  when prices $ plotChart title quotes [Open, Close, High, Low] fname_prices
  when volumes $ plotChart title quotes [Volume] fname_volumes
  where
    statInfo' = statInfo quotes
    withCompany pref =
      if company /= ""
        then pref ++ company
        else ""
    img_suffix = withCompany "_" ++ ".svg"
    fname_prices = "prices" ++ img_suffix
    fname_volumes = "volumes" ++ img_suffix
    title = "Historical Quotes" ++ withCompany " for "
