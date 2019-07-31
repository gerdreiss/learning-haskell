module Params
  ( Params(..)
  , cmdLineParser
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Params =
  Params
    { fname   :: FilePath
    , company :: String
    , prices  :: Bool
    , volumes :: Bool
    }

mkParams :: Parser Params
mkParams = Params <$> fileOpt <*> companyOpt <*> pricesOpt <*> volumesOpt
  where
    fileOpt = strArgument (metavar "FILE" <> help "CSV file name")
    companyOpt = strOption (long "company" <> short 'c' <> help "stock company's name" <> value "")
    pricesOpt = switch (long "prices" <> short 'p' <> help "create file with prices chart")
    volumesOpt = switch (long "volumes" <> short 'v' <> help "create file with volumes chart")

cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts = info (mkParams <**> helper) (fullDesc <> progDesc "Stock quotes data processing")
