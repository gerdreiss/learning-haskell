module LogLevels
  ( LogLevel(..)
  , cmp
  , cmp0
  , cmp1
  ) where

data LogLevel = Error | Warning | Info

-- my favorite solution, inspired by somebody else's solution
instance Enum LogLevel where
    fromEnum Info    = 1
    fromEnum Warning = 2
    fromEnum Error   = 3
    toEnum 1 = Info
    toEnum 2 = Warning
    toEnum 3 = Error

cmp :: LogLevel -> LogLevel -> Ordering
cmp a b = compare (fromEnum a) (fromEnum b)

-- mine, but rather ugly
cmp0 :: LogLevel -> LogLevel -> Ordering
cmp0 Error Error     = EQ
cmp0 Warning Warning = EQ
cmp0 Info Info       = EQ
cmp0 Error _         = GT
cmp0 _ Error         = LT
cmp0 Info _          = LT
cmp0 _ Info          = GT


-- not mine, but more elegant than cmp0
cmp1 :: LogLevel -> LogLevel -> Ordering
cmp1 x y = compare (ord x)  (ord y) where
   ord Error   = 3
   ord Warning = 2
   ord Info    = 1

