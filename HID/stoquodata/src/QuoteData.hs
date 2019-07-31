{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module QuoteData where

import           BoundedEnum           (BoundedEnum)
import           Data.ByteString.Char8 (unpack)
import           Data.Csv              (FromField (..), FromNamedRecord)
import           Data.Fixed            (Fixed, HasResolution (..))
import           Data.Time             (Day, defaultTimeLocale, parseTimeM)
import           GHC.Generics          (Generic)
import           Safe                  (readDef)

data E4

data QField
  = Open
  | Close
  | High
  | Low
  | Volume
  deriving (Show, Enum, Bounded, BoundedEnum)

data QuoteData =
  QuoteData
    { day    :: Day
    , close  :: Fixed4
    , volume :: Fixed4
    , open   :: Fixed4
    , high   :: Fixed4
    , low    :: Fixed4
    }
  deriving (Generic, FromNamedRecord)

type Fixed4 = Fixed E4

instance HasResolution E4 where
  resolution _ = 10000

instance FromField Fixed4 where
  parseField = pure . readDef 0 . unpack

instance FromField Day where
  parseField s = parseTimeM False defaultTimeLocale "%Y/%m/%d" (unpack s)

--
--
field2fun :: QField -> QuoteData -> Fixed4
field2fun Open   = open
field2fun Close  = close
field2fun High   = high
field2fun Low    = low
field2fun Volume = volume
