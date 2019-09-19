module Data.Du where

import           Control.Monad.RWS
import           System.Posix.Types

data DuConfig = DuConfig
  { basePath :: FilePath
  , maxDepth :: Int
  , ext      :: Maybe String
  }

data DuState s = DuState
  { curDepth :: Int
  , curPath  :: FilePath
  , st_field :: s
  }

type DuLog s = [(FilePath, s)]

type DuM s = RWST DuConfig (DuLog s) (DuState s) IO

type DUApp = DuM FileOffset
