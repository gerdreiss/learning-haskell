module LogLevelToString where

import           Data.Time.Clock
import           Data.Time.Format

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

instance Show LogLevel where
    show Error   = "Error"
    show Warning = "Warning"
    show Info    = "Info"

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString (LogEntry t l m) = timeToString t ++ ": " ++ logLevelToString l ++ ": " ++ m

updateLogEntry newMessage entry = entry { message = newMessage }
