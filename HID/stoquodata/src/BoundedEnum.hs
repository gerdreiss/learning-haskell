module BoundedEnum
  ( BoundedEnum(range)
  ) where

import Prelude (Bounded(minBound), Enum(enumFrom))

class (Enum a, Bounded a) => BoundedEnum a where
  range :: [a]
  range = enumFrom minBound
