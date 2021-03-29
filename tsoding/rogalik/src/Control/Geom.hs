module Control.Geom where

import           Data.Geom

(^+^) :: Size -> Size -> Size
(^+^) s1 s2 = s1 <> s2

(^-^) :: Size -> Size -> Size
(^-^) (Size w1 h1) (Size w2 h2) = Size (w1 - w2) (h1 - h2)
