module Locator where

import Algebra

orient :: Turn -> Direction -> Direction
orient TNone = id
orient TLeft = cpred
orient TRight = csucc
orient TAround = cpred . cpred

findTurn :: Direction -> Direction -> Turn
findTurn d1 d2 = head $ filter (\t -> orient t d1 == d2) range
