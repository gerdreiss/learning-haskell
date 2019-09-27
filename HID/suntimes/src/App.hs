module App where

import           Options.Applicative

data AppMode = FileInput FilePath | Interactive
data Params = Params
                AppMode -- mode
                FilePath -- config file

mkParams :: Parser Params
mkParams = Params <$> (fileInput <|> interactive) <*> config
 where
  fileInput = FileInput <$> strOption
    (long "file" <> short 'f' <> metavar "FILENAME" <> help "Input file")
  interactive = flag
    Interactive
    Interactive
    (long "interactive" <> short 'i' <> help "Interactive mode")
  config = strOption
    (  long "conf"
    <> short 'c'
    <> value "config.json"
    <> showDefault
    <> metavar "CONFIGNAME"
    <> help "Configuration file"
    )

